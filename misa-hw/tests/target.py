from abc import ABC, abstractmethod
from dataclasses import astuple, dataclass

from amaranth import Cat, Module, Value
from amaranth.sim import Simulator

from misa_hw.init import CoreInit
from misa_hw.misa_core import CORE_STATE_CODES, CoreState, MisaCore

from memory import MemMap, MemoryBfm, MemoryLatency
from state import CSR_REG_NAMES, MPU_REGION_COUNT, MisaState


HALTED_CODE: int = CORE_STATE_CODES[CoreState.HALTED]

CYCLES_PER_BOUNDARY: int = 512
DEFAULT_MAX_CYCLES: int = 1 << 20

INT_PIN_LEAD: int = 2

INT_PIN_FIRST: int = 1


@dataclass(frozen = True)
class IntPulse:

    boundary: int = None
    after: int = 0
    cycles: int = 1


    def __post_init__(self) -> None:
        if (self.boundary is not None) and (self.boundary < 0):
            raise ValueError(f"IntPulse boundary {self.boundary} is not a boundary index; pass "
                             f"None to anchor to the start of the run")
        if (self.after < 0):
            raise ValueError(f"IntPulse after {self.after} would raise the pin before the cycle it "
                             f"is anchored to, which the run has already left")
        if (self.cycles < 1):
            raise ValueError(f"IntPulse cycles {self.cycles} never raises the pin")


def pin_for_step(step: int) -> IntPulse:
    if (step < 0):
        raise ValueError(f"there is no oracle step {step} to land an interrupt on")
    if (step == 0):
        return IntPulse(None, INT_PIN_FIRST)
    return IntPulse(step - 1)


def pin_schedule(steps: tuple) -> tuple:
    return tuple(pin_for_step(step) for step in steps)


class PinDriver:

    def __init__(self, pulses: tuple) -> None:
        for pulse in pulses:
            if (not isinstance(pulse, IntPulse)):
                raise TypeError(
                    f"{pulse!r} is not an IntPulse; the design side of an interrupt schedule is a "
                    f"pin assertion, not an oracle step index (see pin_for_step)"
                )
        self.pulses: tuple = tuple(pulses)
        self.windows: list = []
        self.fired: list = []
        self.driven: int = 0


    def anchor(self, boundary: int, cycle: int) -> None:
        for pulse in self.pulses:
            if (pulse.boundary == boundary):
                rise: int = cycle + pulse.after
                self.windows.append((rise, rise + pulse.cycles))
                self.fired.append(pulse)


    def at(self, cycle: int) -> int:
        return int(any((rise <= cycle) and (cycle < fall) for rise, fall in self.windows))


    def unfired(self) -> tuple:
        return tuple(pulse for pulse in self.pulses if (pulse not in self.fired))


class RunResult:

    def __init__(self, states: list, bfm: MemoryBfm, cycles: int, boundaries: int = None,
                 boundary_cycles: tuple = (), pin: PinDriver = None) -> None:
        self.states: list = states
        self.bfm: MemoryBfm = bfm
        self.cycles: int = cycles
        self.boundaries: int = (len(states) - int(self.halted)) if (boundaries is None) \
            else boundaries
        self.boundary_cycles: tuple = boundary_cycles
        self.pin: PinDriver = pin


    @property
    def halted(self) -> bool:
        return bool(self.states) and self.states[-1].halted


    def observed(self) -> int:
        return self.boundaries + int(self.halted)


class PackedView:

    def __init__(self, signals: tuple) -> None:
        self.value: Value = Cat(*signals)
        self.widths: tuple = tuple(len(signal) for signal in signals)


    def unpack(self, packed: int) -> list:
        fields: list = []
        for width in self.widths:
            fields.append(packed & ((1 << width) - 1))
            packed >>= width
        return fields


    def pack(self, fields: tuple) -> int:
        packed: int = 0
        offset: int = 0
        for width, field in zip(self.widths, fields):
            packed |= (field & ((1 << width) - 1)) << offset
            offset += width
        return packed


@dataclass
class _BusViews:
    caps: PackedView
    data: PackedView
    request: PackedView
    probe: PackedView


class _RunContext:

    def __init__(self, bfm: MemoryBfm, steps: int, max_cycles: int, record: bool,
                 pin: PinDriver, linger: int) -> None:
        self.bfm: MemoryBfm = bfm
        self.steps: int = steps
        self.max_cycles: int = max_cycles
        self.record: bool = record
        self.pin: PinDriver = pin
        self.linger: int = linger
        self.states: list = []
        self.boundaries: int = 0
        self.boundary_cycles: list = []
        self.cycles: int = 0
        self.lingered: int = 0
        self.failure: str = None


    def boundary(self) -> None:
        self.pin.anchor(self.boundaries, self.cycles)
        self.boundaries += 1
        if (self.record):
            self.boundary_cycles.append(self.cycles)


    def ends_here(self) -> bool:
        return (self.steps is not None) and (self.boundaries > self.steps)


    def wanted(self) -> bool:
        return (self.record) or (self.ends_here())


class Target(ABC):

    name: str
    builds: int


    @abstractmethod
    def run(self, image: bytes, init: CoreInit, steps: int, pin: tuple,
            memmap: MemMap, latency: MemoryLatency = None, reset_extns: int = None,
            max_cycles: int = None, record: bool = True, linger: int = 0) -> RunResult:
        ...


class AmaranthTarget(Target):

    name: str = "amaranth"


    def __init__(self) -> None:
        self.builds: int = 0
        self._cache: dict = {}
        self._context: _RunContext = None


    def _key(self, init: CoreInit, reset_extns: int) -> tuple:
        return (reset_extns, None if (init is None) else astuple(init))


    def _build(self, init: CoreInit, reset_extns: int) -> tuple:
        self.builds += 1
        core: MisaCore = MisaCore(init, reset_extns = reset_extns)
        module: Module = Module()
        module.submodules.core = core

        simulator: Simulator = Simulator(module)
        simulator.add_clock(1e-6)

        views: _BusViews = _BusViews(
            caps = PackedView((core.mem_mapped, core.mem_caps)),
            data = PackedView((core.mem_ready, core.mem_read_data)),
            request = PackedView((
                core.mem_addr, core.mem_request, core.mem_write_enable, core.mem_write_data
            )),
            probe = PackedView((core.state_data, core.boundary_data))
        )

        async def drive(ctx: object) -> None:
            await self._drive(ctx, core, views)

        simulator.add_testbench(drive)
        return (core, simulator)


    async def _drive(self, ctx: object, core: MisaCore, views: _BusViews) -> None:
        context: _RunContext = self._context
        bfm: MemoryBfm = context.bfm
        pin: PinDriver = context.pin
        pin.anchor(None, 0)

        while (True):
            addr: int = ctx.get(core.mem_addr)
            mapped, caps = bfm.responses(addr)
            ctx.set(views.caps.value, views.caps.pack((mapped, caps)))

            settled, request, write, write_data = views.request.unpack(
                ctx.get(views.request.value)
            )
            if (settled != addr):
                context.failure = (
                    f"mem_addr moved from {addr:#06x} to {settled:#06x} when the capability "
                    f"response was driven; the BFM answers the address it saw first, so it "
                    f"cannot be a function of mem_mapped or mem_caps"
                )
                return

            ready, data = bfm.settle(
                context.cycles, addr, bool(request), bool(write), write_data
            )
            ctx.set(views.data.value, views.data.pack((ready, data)))

            state, boundary = views.probe.unpack(ctx.get(views.probe.value))

            if (state == HALTED_CODE):
                if (context.lingered >= context.linger):
                    context.states.append(self._sample(ctx, core, bfm, True))
                    return
                context.lingered += 1
            elif (boundary):
                context.boundary()
                if (context.wanted()):
                    context.states.append(self._sample(ctx, core, bfm, False))
                if (context.ends_here()):
                    return

            level: int = pin.at(context.cycles)
            if (level != pin.driven):
                ctx.set(core.interrupt, level)
                pin.driven = level

            context.cycles += 1
            if (context.cycles > context.max_cycles):
                context.failure = (
                    f"no halt and no further boundary after {context.max_cycles} cycles "
                    f"({context.boundaries} boundaries so far, requested {context.steps}, "
                    f"state_data={state}, mem_addr={addr:#06x}); the core is stuck"
                )
                return
            await ctx.tick()


    def run(self, image: bytes, init: CoreInit, steps: int, pin: tuple,
            memmap: MemMap, latency: MemoryLatency = None, reset_extns: int = None,
            max_cycles: int = None, record: bool = True, linger: int = 0) -> RunResult:
        key: tuple = self._key(init, reset_extns)
        fresh: bool = key not in self._cache
        if (fresh):
            self._cache[key] = self._build(init, reset_extns)
        core, simulator = self._cache[key]

        if (max_cycles is None):
            max_cycles = DEFAULT_MAX_CYCLES if (steps is None) \
                else CYCLES_PER_BOUNDARY * (steps + 2)

        self._context = _RunContext(MemoryBfm(image, memmap, latency), steps, max_cycles, record,
                                    PinDriver(pin), linger)
        if (not fresh):
            simulator.reset()
        simulator.run()

        context: _RunContext = self._context
        if (context.failure):
            raise RuntimeError(f"{self.name}: {context.failure}")

        unfired: tuple = context.pin.unfired()
        if (unfired):
            raise RuntimeError(
                f"{self.name}: {unfired} never raised the pin, because the run passed through "
                f"{context.boundaries} boundaries and the pulse is anchored to one it never "
                f"reached; an interrupt schedule which cannot fire would pass for the wrong reason"
            )

        return RunResult(context.states, context.bfm, context.cycles, context.boundaries,
                         tuple(context.boundary_cycles), context.pin)


    def _sample(self, ctx: object, core: MisaCore, bfm: MemoryBfm, halted: bool) -> MisaState:
        csr_file: object = core.csr_reg_file
        mpu: object = csr_file.mpu_unit

        return MisaState(
            gp_regs = tuple(ctx.get(signal) for signal in core.gp_reg_file.gp_regs),
            csrs = {name: ctx.get(getattr(csr_file, name)) for name in CSR_REG_NAMES},
            mpu_regions = tuple(ctx.get(mpu.regions[slot]) for slot in range(MPU_REGION_COUNT)),
            mpu_index = ctx.get(mpu.index),
            pc = ctx.get(core.pc),
            in_syscall = bool(ctx.get(core.in_syscall)),
            in_interrupt = bool(ctx.get(core.in_interrupt)),
            int_pending = bool(ctx.get(core.int_pending)),
            int_rscratch = ctx.get(core.int_rscratch),
            int_flags = ctx.get(core.int_flags),
            syscall_privs = ctx.get(core.syscall_privs),
            int_privs = ctx.get(core.int_privs),
            halted = halted,
            memory = bfm.snapshot()
        )

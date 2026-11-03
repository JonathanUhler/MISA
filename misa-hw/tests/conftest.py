"""Shared helpers for driving the core against an idealized memory in simulation."""

from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.memory import IdealMemory


def make_image() -> list:
    return [0] * 65536


def put16(img: list, addr: int, word: int) -> None:
    img[addr] = word & 0xFF
    img[addr + 1] = (word >> 8) & 0xFF


def program(words: list, base: int = 0xC000) -> list:
    """Place a list of 16-bit instruction words at `base` and point the reset vector there."""
    img: list = make_image()
    addr: int = base
    for w in words:
        put16(img, addr, w)
        addr += 2
    put16(img, 0xFFFE, base)
    return img


def _connect(m: Module, core: object, mem: object) -> None:
    m.d.comb += [
        mem.mem_addr.eq(core.mem_addr),
        mem.mem_wdata.eq(core.mem_wdata),
        mem.mem_we.eq(core.mem_we),
        mem.mem_req.eq(core.mem_req),
        core.mem_rdata.eq(mem.mem_rdata),
        core.mem_ready.eq(mem.mem_ready),
    ]


def run_until_halt(image: list, extns_reset: int = 0, irq_schedule: dict = None,
                   max_cycles: int = 5000) -> dict:
    """Run a program image until the core halts. Returns a result dict.

    irq_schedule: optional dict {cycle_index: value} driving core.irq at given cycles.
    """
    from misa_hw.core import Core  # lazy: core.py appears in Task 8

    m: Module = Module()
    core: Core = Core(extns_reset=extns_reset)
    mem: IdealMemory = IdealMemory(init=image)
    m.submodules.core = core
    m.submodules.mem = mem
    _connect(m, core, mem)

    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        for i in range(max_cycles):
            if irq_schedule is not None and hasattr(core, "irq") and i in irq_schedule:
                ctx.set(core.irq, irq_schedule[i])
            if ctx.get(core.halted):
                break
            await ctx.tick()
        out["halted"] = ctx.get(core.halted)
        out["halt_code"] = ctx.get(core.halt_code)
        out["cycles"] = i

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out

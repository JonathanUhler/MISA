import pytest
from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.core import Core
from misa_hw.grammar import Extn
from misa_hw.init import CoreInit, CsrInit
from misa_hw.memory import IdealMemory

from testlib import assemble, toolchain_dir


pytestmark = pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")


def _run_irq(image: list, extns: int, raise_at: int = 60, max_cycles: int = 8000) -> dict:
    if extns is None:
        core: Core = Core()                      # canonical reset: EXTNS = 0x0F (interrupt on)
    else:
        pc0: int = image[0xFFFE] | (image[0xFFFF] << 8)
        core = Core(init_data = CoreInit(pc = pc0, csr_init = CsrInit(extns = extns)))
    mem: IdealMemory = IdealMemory(init = image)
    m: Module = Module()
    m.submodules.core = core
    m.submodules.mem = mem
    m.d.comb += [
        mem.mem_addr.eq(core.mem_addr),
        mem.mem_write_data.eq(core.mem_write_data),
        mem.mem_write_enable.eq(core.mem_write_enable),
        mem.mem_request.eq(core.mem_request),
        core.mem_read_data.eq(mem.mem_read_data),
        core.mem_ready.eq(mem.mem_ready),
    ]
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.interrupt, 0)
        for i in range(max_cycles):
            if i == raise_at:
                ctx.set(core.interrupt, 1)       # raise and hold the interrupt pin
            if ctx.get(core.halted):
                break
            await ctx.tick()
        out["halted"] = int(ctx.get(core.halted))
        out["halt_code"] = int(ctx.get(core.halt_code))
        out["in_interrupt"] = int(ctx.get(core.in_interrupt))

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out


_SPIN_HANDLER: str = (
    "        goto _start\n"
    "_handler:\n"
    "        set ra 0xaa\n"
    "        halt ra\n"
)


def test_interrupt_vectors_to_handler_and_enters_context() -> None:
    img: list = assemble(_SPIN_HANDLER, interrupt = "_handler")
    out: dict = _run_irq(img, extns = None)       # EXTNS 0x0F -> interrupt enabled
    assert out["halted"] == 1
    assert out["halt_code"] == 0xAA               # the handler ran
    assert out["in_interrupt"] == 1               # entered (and stayed in) the interrupt context


def test_interrupt_ignored_when_extension_disabled() -> None:
    img: list = assemble(_SPIN_HANDLER, interrupt = "_handler")
    out: dict = _run_irq(img, extns = Extn.DYNAMICHW | Extn.SYSCALL | Extn.PRIVILEGE)  # no interrupt
    assert out["halted"] == 0                     # pin ignored -> the program spins forever


def test_reti_returns_and_clears_context() -> None:
    body: str = "        set rt 0x00\n" + "".join(
        f"        set ra 0x{i:02x}\n" for i in range(1, 25))
    src: str = (
        body
        + "        halt rt\n"
        + "_handler:\n"
        + "        set rt 0xbb\n"
        + "        reti\n"                    # return from interrupt (needs -e interrupt)
    )
    img: list = assemble(src, extns = ("dynamichw", "interrupt"), interrupt = "_handler")
    out: dict = _run_irq(img, extns = None, raise_at = 40)
    assert out["halted"] == 1
    assert out["halt_code"] == 0xBB               # handler ran (set rt) and RETI resumed main
    assert out["in_interrupt"] == 0               # RETI left the interrupt context

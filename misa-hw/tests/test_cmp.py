from amaranth.sim import Simulator, SimulatorContext

from misa_hw.cmp import CmpUnit
from misa_hw.types import Cmp


def taken(flags: int, sel: Cmp) -> int:
    dut: CmpUnit = CmpUnit()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.flags, flags)
        ctx.set(dut.sel, sel)
        await ctx.delay(1e-9)
        out["t"] = ctx.get(dut.taken)

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()
    return out["t"]


def test_always_is_true() -> None:
    assert taken(0b0000, Cmp.ALWAYS) == 1


def test_equal_reads_z() -> None:
    assert taken(0b0001, Cmp.EQUAL) == 1 and taken(0b0000, Cmp.EQUAL) == 0


def test_less_is_n_xor_v() -> None:
    assert taken(0b0100, Cmp.LESS) == 1 and taken(0b1100, Cmp.LESS) == 0


def test_greater_excludes_zero() -> None:
    assert taken(0b0000, Cmp.GREATER) == 1 and taken(0b0001, Cmp.GREATER) == 0


def test_less_equal_includes_zero() -> None:
    assert taken(0b0001, Cmp.LESS_EQUAL) == 1

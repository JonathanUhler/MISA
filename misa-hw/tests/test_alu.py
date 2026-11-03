from amaranth.sim import Simulator, SimulatorContext

from misa_hw.alu import Alu
from misa_hw.types import AluOp


def _eval(op: AluOp, a: int, b: int, carry_in: int = 0) -> dict:
    dut: Alu = Alu()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.op, op)
        ctx.set(dut.a, a)
        ctx.set(dut.b, b)
        ctx.set(dut.carry_in, carry_in)
        await ctx.delay(1e-9)
        out.update(result=ctx.get(dut.result), z=ctx.get(dut.z), c=ctx.get(dut.c),
                   n=ctx.get(dut.n), v=ctx.get(dut.v))

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()
    return out


def test_add_signed_overflow() -> None:
    assert _eval(AluOp.ADD, 0x7F, 0x01) == dict(result=0x80, z=0, c=0, n=1, v=1)


def test_add_unsigned_carry_zero() -> None:
    assert _eval(AluOp.ADD, 0xFF, 0x01) == dict(result=0x00, z=1, c=1, n=0, v=0)


def test_adc_uses_carry_in() -> None:
    assert _eval(AluOp.ADC, 0xFF, 0x00, carry_in=1) == dict(result=0x00, z=1, c=1, n=0, v=0)


def test_sub_borrow_clears_carry() -> None:
    assert _eval(AluOp.SUB, 0x00, 0x01) == dict(result=0xFF, z=0, c=0, n=1, v=0)


def test_sub_signed_overflow() -> None:
    assert _eval(AluOp.SUB, 0x80, 0x01) == dict(result=0x7F, z=0, c=1, n=0, v=1)


def test_sbb_uses_carry_in() -> None:
    assert _eval(AluOp.SBB, 0x05, 0x02, carry_in=0) == dict(result=0x02, z=0, c=1, n=0, v=0)


def test_and_clears_c_and_v() -> None:
    assert _eval(AluOp.AND, 0xF0, 0x0F) == dict(result=0x00, z=1, c=0, n=0, v=0)


def test_xor_sets_n() -> None:
    assert _eval(AluOp.XOR, 0x80, 0x00) == dict(result=0x80, z=0, c=0, n=1, v=0)


def test_rrc_shifts_carry_in_high() -> None:
    assert _eval(AluOp.RRC, 0x00, 0x00, carry_in=1) == dict(result=0x80, z=0, c=0, n=1, v=0)


def test_rrc_shifts_lsb_out_to_carry() -> None:
    assert _eval(AluOp.RRC, 0x01, 0x00, carry_in=0) == dict(result=0x00, z=1, c=1, n=0, v=0)

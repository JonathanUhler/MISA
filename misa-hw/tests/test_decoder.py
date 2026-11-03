from amaranth.sim import Simulator, SimulatorContext

from misa_hw.decoder import Decoder
from misa_hw.types import Opcode


def decode(inst: int) -> dict:
    dut: Decoder = Decoder()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.inst, inst)
        await ctx.delay(1e-9)
        out["opcode"] = ctx.get(dut.out.opcode)
        out["n1"] = ctx.get(dut.out.n1)
        out["n2"] = ctx.get(dut.out.n2)
        out["n3"] = ctx.get(dut.out.n3)
        out["imm8"] = ctx.get(dut.out.imm8)
        out["illegal"] = ctx.get(dut.out.illegal)

    sim: Simulator = Simulator(dut)
    sim.add_testbench(tb)
    sim.run()
    return out


def test_decode_add() -> None:
    d: dict = decode(0x0001)
    assert d["opcode"] == Opcode.ADD
    assert (d["n1"], d["n2"], d["n3"]) == (0, 0, 0)
    assert d["illegal"] == 0


def test_decode_set() -> None:
    d: dict = decode(0x2A19)
    assert d["opcode"] == Opcode.SET
    assert d["n1"] == 1 and d["imm8"] == 0x2A


def test_decode_illegal_csr() -> None:
    assert decode(0x600C)["illegal"] == 1  # RSR with CSR field 0x6


def test_decode_illegal_cmp() -> None:
    assert decode(0x700E)["illegal"] == 1  # JAL with cmp field 0b111


def test_decode_valid_rsr_saddr() -> None:
    assert decode(0x100C)["illegal"] == 0  # RSR SADDR

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.csr import SpecFile
from misa_hw.types import Csr


def _run(dut: SpecFile, tb_body: object) -> dict:
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        await tb_body(ctx, out)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    return out


def test_general_write_read_back() -> None:
    dut: SpecFile = SpecFile()

    async def body(ctx: SimulatorContext, out: dict) -> None:
        ctx.set(dut.waddr, Csr.SADDR)
        ctx.set(dut.wdata, 0x1234)
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.we, 0)
        ctx.set(dut.raddr, Csr.SADDR)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    assert _run(dut, body)["v"] == 0x1234


def test_flags_update_via_flags_port() -> None:
    dut: SpecFile = SpecFile()

    async def body(ctx: SimulatorContext, out: dict) -> None:
        ctx.set(dut.flags_we, 1)
        ctx.set(dut.flags_in, 0b1010)
        await ctx.tick()
        ctx.set(dut.flags_we, 0)
        ctx.set(dut.raddr, Csr.FLAGS)
        await ctx.delay(1e-9)
        out["rdata"] = ctx.get(dut.rdata)
        out["flags_out"] = ctx.get(dut.flags_out)

    r: dict = _run(dut, body)
    assert r["rdata"] == 0x000A and r["flags_out"] == 0b1010


def test_extns_write_masks_bit0_when_dynamic() -> None:
    dut: SpecFile = SpecFile()  # reset EXTNS 0x0F, Dynamic Hardware (bit 0) set

    async def body(ctx: SimulatorContext, out: dict) -> None:
        ctx.set(dut.waddr, Csr.EXTNS)
        ctx.set(dut.wdata, 0x0000)  # try to clear everything
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.we, 0)
        ctx.set(dut.raddr, Csr.EXTNS)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    assert _run(dut, body)["v"] == 0x0001  # bits 1-3 cleared, bit 0 pinned


def test_extns_write_ignored_when_not_dynamic() -> None:
    dut: SpecFile = SpecFile(extns_reset=0x00)  # Dynamic Hardware off

    async def body(ctx: SimulatorContext, out: dict) -> None:
        ctx.set(dut.waddr, Csr.EXTNS)
        ctx.set(dut.wdata, 0x000E)
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.we, 0)
        ctx.set(dut.raddr, Csr.EXTNS)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    assert _run(dut, body)["v"] == 0x0000


def test_extns_reset_default() -> None:
    dut: SpecFile = SpecFile()

    async def body(ctx: SimulatorContext, out: dict) -> None:
        ctx.set(dut.raddr, Csr.EXTNS)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    assert _run(dut, body)["v"] == 0x000F

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.regfile import RegFile


def test_write_then_read_back() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.waddr, 5)
        ctx.set(dut.wdata, 0xAB)
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.we, 0)
        ctx.set(dut.raddr, 5)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0xAB


def test_r0_is_hardwired_zero() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.waddr, 0)
        ctx.set(dut.wdata, 0x7F)
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.we, 0)
        ctx.set(dut.raddr, 0)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0x00


def test_write_disabled_leaves_target_unchanged() -> None:
    dut: RegFile = RegFile()
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(dut.waddr, 7)
        ctx.set(dut.wdata, 0x22)
        ctx.set(dut.we, 1)
        await ctx.tick()
        ctx.set(dut.wdata, 0x33)
        ctx.set(dut.we, 0)
        await ctx.tick()
        ctx.set(dut.raddr, 7)
        await ctx.delay(1e-9)
        out["v"] = ctx.get(dut.rdata)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0x22

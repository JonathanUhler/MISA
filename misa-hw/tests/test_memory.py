from amaranth.sim import Simulator, SimulatorContext

from misa_hw.memory import IdealMemory


async def txn(ctx: SimulatorContext, dut: IdealMemory, addr: int, we: int, wdata: int = 0) -> int:
    ctx.set(dut.mem_addr, addr)
    ctx.set(dut.mem_we, we)
    ctx.set(dut.mem_wdata, wdata)
    ctx.set(dut.mem_req, 1)
    await ctx.tick()
    ctx.set(dut.mem_req, 0)
    for _ in range(16):
        if ctx.get(dut.mem_ready):
            break
        await ctx.tick()
    assert ctx.get(dut.mem_ready) == 1
    data: int = ctx.get(dut.mem_rdata)
    await ctx.tick()  # leave the response state (commits a write)
    return data


def test_read_returns_preloaded_byte() -> None:
    img: list = [0] * 65536
    img[0x1000] = 0x42
    dut: IdealMemory = IdealMemory(init=img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        out["v"] = await txn(ctx, dut, 0x1000, we=0)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0x42


def test_write_then_read_back() -> None:
    dut: IdealMemory = IdealMemory(init=[])
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        await txn(ctx, dut, 0x2000, we=1, wdata=0x99)
        out["v"] = await txn(ctx, dut, 0x2000, we=0)

    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0x99

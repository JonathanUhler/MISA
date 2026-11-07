import random

from amaranth.sim import Simulator, SimulatorContext

from misa_hw.memory import IdealMemory


async def _txn(ctx: SimulatorContext, dut: IdealMemory, addr: int, we: int = 0,
               wdata: int = 0, timeout: int = 8) -> int:
    ctx.set(dut.mem_addr, addr)
    ctx.set(dut.mem_write_enable, we)
    ctx.set(dut.mem_write_data, wdata)
    ctx.set(dut.mem_request, 1)
    await ctx.tick()
    ctx.set(dut.mem_request, 0)
    data: int = None
    for _ in range(timeout):
        if ctx.get(dut.mem_ready):
            data = int(ctx.get(dut.mem_read_data))
            break
        await ctx.tick()
    assert data is not None, "memory never asserted mem_ready"
    await ctx.tick()
    return data


def _sim(dut: IdealMemory, tb) -> None:
    sim: Simulator = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()


def test_read_preloaded_byte() -> None:
    img: list = [0] * 65536
    img[0x1234] = 0x5A
    dut: IdealMemory = IdealMemory(init = img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        out["v"] = await _txn(ctx, dut, 0x1234, we = 0)

    _sim(dut, tb)
    assert out["v"] == 0x5A


def test_write_then_read_back() -> None:
    dut: IdealMemory = IdealMemory(init = [0] * 65536)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        await _txn(ctx, dut, 0x2000, we = 1, wdata = 0x99)
        out["v"] = await _txn(ctx, dut, 0x2000, we = 0)

    _sim(dut, tb)
    assert out["v"] == 0x99


def test_fuzz_against_shadow() -> None:
    dut: IdealMemory = IdealMemory(init = [0] * 65536)
    rng: random.Random = random.Random(11)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        shadow: dict = {}
        bad: list = []
        for _ in range(200):
            addr: int = rng.randint(0, 0xFFFF)
            if rng.randint(0, 1):
                data: int = rng.randint(0, 255)
                await _txn(ctx, dut, addr, we = 1, wdata = data)
                shadow[addr] = data
            else:
                got: int = await _txn(ctx, dut, addr, we = 0)
                exp: int = shadow.get(addr, 0)
                if got != exp:
                    bad.append((addr, got, exp))
        out["bad"] = bad

    _sim(dut, tb)
    assert out["bad"] == [], f"memory fuzz mismatch (addr,got,exp): {out['bad'][:10]}"

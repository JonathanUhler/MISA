import random

from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.qspi import QspiController, QspiMemModel, connect_qspi


def _build(init: list):
    m: Module = Module()
    ctrl: QspiController = QspiController()
    model: QspiMemModel = QspiMemModel(init = init)
    m.submodules.ctrl = ctrl
    m.submodules.model = model
    connect_qspi(m, ctrl, model)
    return m, ctrl


async def _txn(ctx: SimulatorContext, ctrl: QspiController, addr: int, we: int,
               wdata: int = 0) -> int:
    ctx.set(ctrl.mem_addr, addr)
    ctx.set(ctrl.mem_write_enable, we)
    ctx.set(ctrl.mem_write_data, wdata)
    ctx.set(ctrl.mem_request, 1)
    await ctx.tick()
    ctx.set(ctrl.mem_request, 0)
    for _ in range(64):
        if ctx.get(ctrl.mem_ready):
            break
        await ctx.tick()
    assert ctx.get(ctrl.mem_ready) == 1, "qspi controller never asserted mem_ready"
    data: int = int(ctx.get(ctrl.mem_read_data))
    await ctx.tick()
    return data


def _run(m: Module, tb) -> None:
    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()


def test_read_preloaded_bytes() -> None:
    img: list = [0] * 65536
    img[0x1000] = 0x42   # psram region (< flash_base 0x8000)
    img[0xC000] = 0x7E   # flash region (>= 0x8000)
    m, ctrl = _build(img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        out["psram"] = await _txn(ctx, ctrl, 0x1000, we = 0)
        out["flash"] = await _txn(ctx, ctrl, 0xC000, we = 0)

    _run(m, tb)
    assert out["psram"] == 0x42
    assert out["flash"] == 0x7E


def test_write_then_read_back() -> None:
    m, ctrl = _build([0] * 65536)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        await _txn(ctx, ctrl, 0x2000, we = 1, wdata = 0x99)
        out["v"] = await _txn(ctx, ctrl, 0x2000, we = 0)

    _run(m, tb)
    assert out["v"] == 0x99


def test_fuzz_against_shadow() -> None:
    m, ctrl = _build([0] * 65536)
    rng: random.Random = random.Random(13)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        shadow: dict = {}
        bad: list = []
        for _ in range(60):
            addr: int = rng.randint(0, 0xFFFF)
            if rng.randint(0, 1):
                data: int = rng.randint(0, 255)
                await _txn(ctx, ctrl, addr, we = 1, wdata = data)
                shadow[addr] = data
            else:
                got: int = await _txn(ctx, ctrl, addr, we = 0)
                exp: int = shadow.get(addr, 0)
                if got != exp:
                    bad.append((addr, got, exp))
        out["bad"] = bad

    _run(m, tb)
    assert out["bad"] == [], f"qspi fuzz mismatch (addr,got,exp): {out['bad'][:10]}"

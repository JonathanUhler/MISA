import os
import subprocess
import sys
import tempfile

import pytest
from amaranth import Module
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.qspi import QspiController, QspiMemModel, connect_qspi
from misa_hw.core import Core

from conftest import assemble, toolchain_dir

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "cosim"))
from harness import spec_exe, parse_state, REG_NAMES  # noqa: E402


def _build(init: list) -> (Module, QspiController):
    m: Module = Module()
    ctrl: QspiController = QspiController()
    model: QspiMemModel = QspiMemModel(init=init)
    m.submodules.ctrl = ctrl
    m.submodules.model = model
    connect_qspi(m, ctrl, model)
    return m, ctrl


async def _txn(ctx: SimulatorContext, ctrl: QspiController, addr: int, we: int,
               wdata: int = 0) -> int:
    ctx.set(ctrl.mem_addr, addr)
    ctx.set(ctrl.mem_we, we)
    ctx.set(ctrl.mem_wdata, wdata)
    ctx.set(ctrl.mem_req, 1)
    await ctx.tick()
    ctx.set(ctrl.mem_req, 0)
    for _ in range(64):
        if ctx.get(ctrl.mem_ready):
            break
        await ctx.tick()
    assert ctx.get(ctrl.mem_ready) == 1
    data: int = ctx.get(ctrl.mem_rdata)
    await ctx.tick()
    return data


def test_qspi_read_returns_preloaded_byte() -> None:
    img: list = [0] * 65536
    img[0x1000] = 0x42   # psram region (< 0x8000)
    img[0xC000] = 0x7E   # flash region (>= 0x8000)
    m, ctrl = _build(img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        out["psram"] = await _txn(ctx, ctrl, 0x1000, we=0)
        out["flash"] = await _txn(ctx, ctrl, 0xC000, we=0)

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["psram"] == 0x42
    assert out["flash"] == 0x7E


def test_qspi_write_then_read_back() -> None:
    m, ctrl = _build([])
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        await _txn(ctx, ctrl, 0x2000, we=1, wdata=0x99)
        out["v"] = await _txn(ctx, ctrl, 0x2000, we=0)

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["v"] == 0x99


@pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")
def test_core_runs_program_through_qspi() -> None:
    # The basic program, assembled and fetched over QSPI.
    img: list = assemble("""
        set ra 0x2a
        halt ra
    """)

    m: Module = Module()
    core: Core = Core()
    ctrl: QspiController = QspiController()
    model: QspiMemModel = QspiMemModel(init=img)
    m.submodules.core = core
    m.submodules.ctrl = ctrl
    m.submodules.model = model
    m.d.comb += [
        ctrl.mem_addr.eq(core.mem_addr),
        ctrl.mem_wdata.eq(core.mem_wdata),
        ctrl.mem_we.eq(core.mem_we),
        ctrl.mem_req.eq(core.mem_req),
        core.mem_rdata.eq(ctrl.mem_rdata),
        core.mem_ready.eq(ctrl.mem_ready),
    ]
    connect_qspi(m, ctrl, model)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.irq, 0)
        for _ in range(20000):
            if ctx.get(core.halted):
                break
            await ctx.tick()
        out["halted"] = ctx.get(core.halted)
        out["halt_code"] = ctx.get(core.halt_code)

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert out["halted"] == 1
    assert out["halt_code"] == 0x2A


@pytest.mark.skipif(spec_exe() is None, reason="misa-spec-exe oracle not found")
@pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")
def test_qspi_core_matches_oracle_from_reset() -> None:
    # Exercise flash fetches plus a PSRAM store/load, then compare the whole register file and pc to
    # the oracle's canonical q0 run on the same image.
    img: list = assemble("""
        set ra 0x05
        set rb 0x03
        add rc ra rb
        set rd 0x02
        set re 0x10
        st rc rd 0
        ld rf rd 0
        halt rf
    """)

    m: Module = Module()
    core: Core = Core(extns_reset=0x00)
    ctrl: QspiController = QspiController()
    model: QspiMemModel = QspiMemModel(init=list(img))
    m.submodules.core = core
    m.submodules.ctrl = ctrl
    m.submodules.model = model
    m.d.comb += [
        ctrl.mem_addr.eq(core.mem_addr), ctrl.mem_wdata.eq(core.mem_wdata),
        ctrl.mem_we.eq(core.mem_we), ctrl.mem_req.eq(core.mem_req),
        core.mem_rdata.eq(ctrl.mem_rdata), core.mem_ready.eq(ctrl.mem_ready),
    ]
    connect_qspi(m, ctrl, model)
    hw: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(core.irq, 0)
        for _ in range(20000):
            if ctx.get(core.halted):
                break
            await ctx.tick()
        hw["halted"] = bool(ctx.get(core.halted))
        hw["pc"] = ctx.get(core._pc)
        hw["regs"] = {name: (0 if i == 0 else ctx.get(core._regfile.regs[i]))
                      for i, name in enumerate(REG_NAMES)}

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()

    with tempfile.NamedTemporaryFile(suffix=".bin", delete=False) as f:
        f.write(bytes(img))
        path: str = f.name
    try:
        res: subprocess.CompletedProcess = subprocess.run(
            [spec_exe(), "q0", path, "-", "0"], capture_output=True, text=True)
        assert res.returncode == 0, res.stderr
        ref: dict = parse_state(res.stdout)
    finally:
        os.unlink(path)

    assert hw["halted"] == ref["halted"]
    assert hw["pc"] == ref["pc"]
    for name in REG_NAMES:
        assert hw["regs"][name] == ref["registers"].get(name, 0), f"reg {name} mismatch"

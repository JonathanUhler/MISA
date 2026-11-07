import pytest
from amaranth import Module, Signal
from amaranth.back import rtlil
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.qspi import QspiMemModel
from misa_hw.top import TtUmMisa, _SD_BITS, UIO_CS0, UIO_CS1, UIO_CS2, UIO_SCK, UO_HALTED

from testlib import put16, assemble, toolchain_dir


def _board(img: list):
    m: Module = Module()
    top: TtUmMisa = TtUmMisa(standalone = False)
    model: QspiMemModel = QspiMemModel(init = img)
    m.submodules.top = top
    m.submodules.model = model
    m.d.comb += [
        model.cs0.eq(top.uio_out[UIO_CS0]),
        model.cs1.eq(top.uio_out[UIO_CS1]),
        model.cs2.eq(top.uio_out[UIO_CS2]),
        model.sck.eq(top.uio_out[UIO_SCK]),
    ]
    for lane, bit in enumerate(_SD_BITS):
        bus: Signal = Signal(name = f"sd{lane}")
        with m.If(top.uio_oe[bit]):
            m.d.comb += bus.eq(top.uio_out[bit])
        with m.Elif(model.sd_oe[lane]):
            m.d.comb += bus.eq(model.sd_o[lane])
        m.d.comb += [top.uio_in[bit].eq(bus), model.sd_i[lane].eq(bus)]
    return m, top


def test_elaborates_standalone() -> None:
    assert len(rtlil.convert(TtUmMisa(standalone = True))) > 0


def test_elaborates_non_standalone() -> None:
    assert len(rtlil.convert(TtUmMisa(standalone = False))) > 0


def test_board_runs_program() -> None:
    img: list = [0] * 65536
    put16(img, 0x0000, 0x2A19)   # SET RA 0x2A
    put16(img, 0x0002, 0x0010)   # HALT RA
    img[0xFFFE] = 0x00
    img[0xFFFF] = 0x00           # reset vector -> 0x0000
    m, top = _board(img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(top.ui_in, 0)
        for _ in range(4000):
            await ctx.tick()
        out["uo"] = int(ctx.get(top.uo_out))

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert (out["uo"] >> UO_HALTED) & 1 == 1     # halted status bit
    assert out["uo"] & 0x7F == 0x2A              # low 7 bits of the HALT debug code


@pytest.mark.skipif(toolchain_dir() is None, reason="misa-as toolchain not found")
def test_board_interrupt_from_ui_in() -> None:
    img: list = assemble(
        "        goto _start\n"
        "handler:\n"
        "        set ra 0x55\n"
        "        halt ra\n",
        interrupt = "handler",
    )
    m, top = _board(img)
    out: dict = {}

    async def tb(ctx: SimulatorContext) -> None:
        ctx.set(top.ui_in, 0)
        for i in range(6000):
            if i == 200:
                ctx.set(top.ui_in, 1)   # raise the interrupt pin and hold it
            await ctx.tick()
        out["uo"] = int(ctx.get(top.uo_out))

    sim: Simulator = Simulator(m)
    sim.add_clock(1e-6)
    sim.add_testbench(tb)
    sim.run()
    assert (out["uo"] >> UO_HALTED) & 1 == 1     # handler ran and the core halted
    assert out["uo"] & 0x7F == 0x55              # the handler's HALT debug code

from amaranth import Module, Signal
from amaranth.hdl import ClockDomain
from amaranth.sim import Simulator, SimulatorContext

from misa_hw.top import TtUmMisa, _SD_BITS, UIO_CS0, UIO_CS1, UIO_CS2, UIO_SCK
from misa_hw.qspi import QspiMemModel


def test_elaborates_to_verilog() -> None:
    import scripts.elaborate as el
    text: str = el.emit()
    assert "module tt_um_misa" in text


def _connect_board(m: Module, top: TtUmMisa, model: QspiMemModel) -> None:
    """Mirror the carrier board: chip selects/clock out to the model, data lines shared."""
    m.d.comb += [
        model.cs0.eq(top.uio_out[UIO_CS0]),
        model.cs1.eq(top.uio_out[UIO_CS1]),
        model.cs2.eq(top.uio_out[UIO_CS2]),
        model.sck.eq(top.uio_out[UIO_SCK]),
    ]
    uio_in: Signal = Signal(8)
    for lane, bit in enumerate(_SD_BITS):
        line: Signal = Signal(name=f"sd{lane}_line")
        # Chip drives when its oe is set; otherwise the model drives the shared line.
        with m.If(top.uio_oe[bit]):
            m.d.comb += line.eq(top.uio_out[bit])
        with m.Elif(model.sd_oe[lane]):
            m.d.comb += line.eq(model.sd_o[lane])
        m.d.comb += [uio_in[bit].eq(line), model.sd_i[lane].eq(line)]
    m.d.comb += top.uio_in.eq(uio_in)


def test_program_runs_end_to_end_on_pins() -> None:
    img: list = [0] * 65536
    img[0xFFFE], img[0xFFFF] = 0x00, 0xC0   # reset vector -> 0xC000
    img[0xC000], img[0xC001] = 0x19, 0x2A   # SET RA 0x2A
    img[0xC002], img[0xC003] = 0x10, 0x00   # HALT RA

    m: Module = Module()
    # The board owns the clock domain so the chip and the external memory model share it.
    cd: ClockDomain = ClockDomain("sync")
    m.domains.sync = cd
    board_clk: Signal = Signal()
    board_rst_n: Signal = Signal()
    m.d.comb += [cd.clk.eq(board_clk), cd.rst.eq(~board_rst_n)]
    top: TtUmMisa = TtUmMisa(standalone=False)
    model: QspiMemModel = QspiMemModel(init=img)
    m.submodules.top = top
    m.submodules.model = model
    _connect_board(m, top, model)
    out: dict = {"oe_bit1_seen": set()}

    async def tb(ctx: SimulatorContext) -> None:
        async def cycle() -> None:
            ctx.set(board_clk, 1)
            await ctx.delay(0.5e-6)
            ctx.set(board_clk, 0)
            await ctx.delay(0.5e-6)

        ctx.set(top.ena, 1)
        ctx.set(top.ui_in, 0)
        ctx.set(board_clk, 0)
        # Hold reset for two cycles, then release.
        ctx.set(board_rst_n, 0)
        await cycle()
        await cycle()
        ctx.set(board_rst_n, 1)
        for _ in range(20000):
            # CS0 and SCK are always-driven outputs.
            assert ctx.get(top.uio_oe[UIO_CS0]) == 1
            assert ctx.get(top.uio_oe[UIO_SCK]) == 1
            out["oe_bit1_seen"].add(ctx.get(top.uio_oe[_SD_BITS[0]]))
            if ctx.get(top.uo_out) == 0x2A and ctx.get(top.uio_out[UIO_CS0]) == 1:
                # Halted (debug byte present) and bus idle.
                break
            await cycle()
        out["uo"] = ctx.get(top.uo_out)

    sim: Simulator = Simulator(m)
    sim.add_testbench(tb)
    sim.run()
    assert out["uo"] == 0x2A
    # The data line was both driven (writes/command) and released (read turnaround).
    assert out["oe_bit1_seen"] == {0, 1}

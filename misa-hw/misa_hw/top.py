"""TinyTapeout top-level wrapper for the MISA core.

Presents the standard TinyTapeout interface (ui_in, uo_out, uio_in/uio_out/uio_oe, ena, clk, rst_n)
and wires it to the core and the QSPI memory controller per the design's pin map:

    uio[0] CS0 (flash)   uio[1] SD0   uio[2] SD1   uio[3] SCK
    uio[4] SD2           uio[5] SD3   uio[6] CS1   uio[7] CS2

    ui_in[0]  interrupt request      uo_out[7:0] HALT debug byte (the RS operand of HALT)

The active-low reset rst_n drives the synchronous domain's reset; clk is the implicit domain clock.
"""

from amaranth import Module
from amaranth.hdl import ClockDomain
from amaranth.lib import wiring
from amaranth.lib.wiring import In, Out

from .core import Core
from .qspi import QspiController


# Bidirectional-port bit assignments.
UIO_CS0: int = 0
UIO_SD0: int = 1
UIO_SD1: int = 2
UIO_SCK: int = 3
UIO_SD2: int = 4
UIO_SD3: int = 5
UIO_CS1: int = 6
UIO_CS2: int = 7
_SD_BITS: tuple = (UIO_SD0, UIO_SD1, UIO_SD2, UIO_SD3)  # data lines SD0..SD3 -> uio bits


class TtUmMisa(wiring.Component):
    ui_in: In(8)
    uo_out: Out(8)
    uio_in: In(8)
    uio_out: Out(8)
    uio_oe: Out(8)
    ena: In(1)
    clk: In(1)
    rst_n: In(1)

    def __init__(self, flash_base: int = 0x8000, standalone: bool = True) -> None:
        # standalone=True defines the sync domain from the clk/rst_n ports (for tapeout and Verilog
        # emission). standalone=False uses an ambient sync domain supplied by a parent (used when a
        # simulation testbench clocks the whole board, including an external memory model).
        self._flash_base: int = flash_base
        self._standalone: bool = standalone
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        if self._standalone:
            # The TinyTapeout harness supplies the clock and an active-low reset.
            cd: ClockDomain = ClockDomain("sync")
            m.domains.sync = cd
            m.d.comb += [cd.clk.eq(self.clk), cd.rst.eq(~self.rst_n)]

        core: Core = Core()
        ctrl: QspiController = QspiController(flash_base=self._flash_base)
        m.submodules.core = core
        m.submodules.ctrl = ctrl

        # Core <-> memory controller (single-byte port contract).
        m.d.comb += [
            ctrl.mem_addr.eq(core.mem_addr),
            ctrl.mem_wdata.eq(core.mem_wdata),
            ctrl.mem_we.eq(core.mem_we),
            ctrl.mem_req.eq(core.mem_req),
            core.mem_rdata.eq(ctrl.mem_rdata),
            core.mem_ready.eq(ctrl.mem_ready),
        ]

        # Chip selects and clock are always-driven outputs.
        m.d.comb += [
            self.uio_out[UIO_CS0].eq(ctrl.cs0),
            self.uio_out[UIO_CS1].eq(ctrl.cs1),
            self.uio_out[UIO_CS2].eq(ctrl.cs2),
            self.uio_out[UIO_SCK].eq(ctrl.sck),
            self.uio_oe[UIO_CS0].eq(1),
            self.uio_oe[UIO_CS1].eq(1),
            self.uio_oe[UIO_CS2].eq(1),
            self.uio_oe[UIO_SCK].eq(1),
        ]

        # Four bidirectional data lines.
        for lane, bit in enumerate(_SD_BITS):
            m.d.comb += [
                self.uio_out[bit].eq(ctrl.sd_o[lane]),
                self.uio_oe[bit].eq(ctrl.sd_oe[lane]),
                ctrl.sd_i[lane].eq(self.uio_in[bit]),
            ]

        # Interrupt pin and debug output.
        m.d.comb += [
            core.irq.eq(self.ui_in[0]),
            self.uo_out.eq(core.halt_code),
        ]

        return m

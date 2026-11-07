from amaranth import ClockDomain, Module
from amaranth.lib.wiring import Component, In, Out

from .core import Core
from .qspi import QspiController


UIO_CS0: int = 0
UIO_SD0: int = 1
UIO_SD1: int = 2
UIO_SCK: int = 3
UIO_SD2: int = 4
UIO_SD3: int = 5
UIO_CS1: int = 6
UIO_CS2: int = 7
_SD_BITS: tuple = (UIO_SD0, UIO_SD1, UIO_SD2, UIO_SD3)

UO_HALTED: int = 7     # uo_out[7] = halted status; uo_out[6:0] = low 7 bits of the HALT debug code


class TtUmMisa(Component):

    ui_in: In(8)
    uo_out: Out(8)
    uio_in: In(8)
    uio_out: Out(8)
    uio_oe: Out(8)
    ena: In(1)
    clk: In(1)
    rst_n: In(1)


    def __init__(self, flash_base: int = 0x8000, standalone: bool = True) -> None:
        self.flash_base: int = flash_base
        self.standalone: bool = standalone
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        if (self.standalone):
            clock_domain: ClockDomain = ClockDomain("sync")
            m.domains.sync = clock_domain
            m.d.comb += [clock_domain.clk.eq(self.clk), clock_domain.rst.eq(~self.rst_n)]

        core: Core = Core()
        ctrl: QspiController = QspiController(flash_base = self.flash_base)
        m.submodules.core = core
        m.submodules.ctrl = ctrl

        m.d.comb += [
            ctrl.mem_addr.eq(core.mem_addr),
            ctrl.mem_write_data.eq(core.mem_write_data),
            ctrl.mem_write_enable.eq(core.mem_write_enable),
            ctrl.mem_request.eq(core.mem_request),
            core.mem_read_data.eq(ctrl.mem_read_data),
            core.mem_ready.eq(ctrl.mem_ready)
        ]

        m.d.comb += [
            self.uio_out[UIO_CS0].eq(ctrl.cs0),
            self.uio_out[UIO_CS1].eq(ctrl.cs1),
            self.uio_out[UIO_CS2].eq(ctrl.cs2),
            self.uio_out[UIO_SCK].eq(ctrl.sck),
            self.uio_oe[UIO_CS0].eq(1),
            self.uio_oe[UIO_CS1].eq(1),
            self.uio_oe[UIO_CS2].eq(1),
            self.uio_oe[UIO_SCK].eq(1)
        ]

        for lane, bit in enumerate(_SD_BITS):
            m.d.comb += [
                self.uio_out[bit].eq(ctrl.sd_o[lane]),
                self.uio_oe[bit].eq(ctrl.sd_oe[lane]),
                ctrl.sd_i[lane].eq(self.uio_in[bit])
            ]

        m.d.comb += [
            core.interrupt.eq(self.ui_in[0]),
            self.uo_out[0:7].eq(core.halt_code[0:7]),
            self.uo_out[UO_HALTED].eq(core.halted)
        ]

        return m

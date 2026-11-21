from amaranth import C, Cat, ClockDomain, ClockSignal, Module, Mux, ResetSignal, Value
from amaranth.lib.cdc import FFSynchronizer
from amaranth.lib.enum import IntEnum
from amaranth.lib.wiring import Component, In, Out

from .init import CoreInit
from .misa_core import MisaCore
from .qspi_controller import TCEM_PERIODS, ChipSelect, QspiController


# These are a property of the Pmod's architecture/PCB, and cannot be chosen.
UIO_SCK: int = 3
UIO_SD: tuple = (1, 2, 4, 5)
UIO_CS: tuple = (0, 6, 7)

UI_INTERRUPT: int = 0
UI_BUS_GRANTED: int = 1
UI_DEVICE_SELECT: slice = slice(2, 4)
UI_VIEW: slice = slice(4, 7)


class DiagView(IntEnum, shape = 3):
    CORE_STATUS   = 0
    CAUSE_LO      = 1
    CAUSE_HI      = 2
    MEMORY_STATUS = 3
    PC_LO         = 4
    PC_HI         = 5
    FAULT_LO      = 6
    FAULT_HI      = 7


class TtUmMisa(Component):

    ui_in: In(8)
    uio_in: In(8)
    ena: In(1)  # Required for Tiny Tapeout, but never used
    clk: In(1)
    rst_n: In(1)

    uo_out: Out(8)
    uio_out: Out(8)
    uio_oe: Out(8)


    def __init__(self, init_data: CoreInit = None, reset_extns: int = None,
                 tcem_periods: int = TCEM_PERIODS):
        self.misa_core: MisaCore = MisaCore(init_data, reset_extns)
        self.qspi_controller: QspiController = QspiController(tcem_periods)
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_clocking(m)

        m.submodules.misa_core = self.misa_core
        m.submodules.qspi_controller = self.qspi_controller

        self.elaborate_memory_bus(m)
        self.elaborate_pmod(m)
        self.elaborate_views(m)

        return m


    def elaborate_clocking(self, m: Module) -> None:
        m.domains.sync = ClockDomain()
        m.d.comb += [
            ClockSignal("sync").eq(self.clk),
            ResetSignal("sync").eq(~self.rst_n)
        ]


    def elaborate_memory_bus(self, m: Module) -> None:
        m.d.comb += [
            self.qspi_controller.mem_addr.eq(self.misa_core.mem_addr),
            self.qspi_controller.mem_write_data.eq(self.misa_core.mem_write_data),
            self.qspi_controller.mem_write_enable.eq(self.misa_core.mem_write_enable),
            self.qspi_controller.mem_request.eq(self.misa_core.mem_request),

            self.misa_core.mem_ready.eq(self.qspi_controller.mem_ready),
            self.misa_core.mem_read_data.eq(self.qspi_controller.mem_read_data),
            self.misa_core.mem_mapped.eq(self.qspi_controller.mem_mapped),
            self.misa_core.mem_caps.eq(self.qspi_controller.mem_caps),

            self.misa_core.interrupt.eq(self.ui_in[UI_INTERRUPT])
        ]

        m.submodules.bus_granted_sync = FFSynchronizer(
            self.ui_in[UI_BUS_GRANTED], self.qspi_controller.bus_granted, init = 0
        )


    def elaborate_pmod(self, m: Module) -> None:
        with m.Switch(self.ui_in[UI_DEVICE_SELECT]):
            for code, selected in enumerate((
                ChipSelect.RAM_A, ChipSelect.RAM_B, ChipSelect.FLASH, ChipSelect.RAM_A
            )):
                with m.Case(code):
                    m.d.comb += self.qspi_controller.device_select.eq(selected)

        value: list = [C(0, 1)] * len(self.uio_out)
        drive: list = [C(0, 1)] * len(self.uio_oe)

        value[UIO_SCK], drive[UIO_SCK] = self.qspi_controller.sck, C(1, 1)
        for index, pin in enumerate(UIO_CS):
            value[pin], drive[pin] = self.qspi_controller.cs_n[index], C(1, 1)
        for index, pin in enumerate(UIO_SD):
            value[pin], drive[pin] = \
                self.qspi_controller.sd_o[index], self.qspi_controller.sd_oe[index]

        m.d.comb += [
            self.uio_out.eq(Cat(*value)),
            self.uio_oe.eq(Mux(self.qspi_controller.bus_claim, Cat(*drive), 0)),
            self.qspi_controller.sd_i.eq(Cat(*[self.uio_in[pin] for pin in UIO_SD]))
        ]


    def elaborate_views(self, m: Module) -> None:
        views: dict = {
            DiagView.CORE_STATUS: Cat(
                self.misa_core.state_data,
                self.misa_core.in_syscall_data,
                self.misa_core.in_interrupt_data,
                self.misa_core.int_pending_data
            ),
            DiagView.CAUSE_LO: self.misa_core.cause_data[0:8],
            DiagView.CAUSE_HI: self.misa_core.cause_data[8:16],
            DiagView.MEMORY_STATUS: Cat(
                self.qspi_controller.state_data,
                self.qspi_controller.device_select,
                self.qspi_controller.bus_claim,
                self.misa_core.user_mode_data
            ),
            DiagView.PC_LO: self.misa_core.pc_data[0:8],
            DiagView.PC_HI: self.misa_core.pc_data[8:16],
            DiagView.FAULT_LO: self.misa_core.fault_data[0:8],
            DiagView.FAULT_HI: self.misa_core.fault_data[8:16]
        }

        with m.Switch(self.ui_in[UI_VIEW]):
            for view, value in views.items():
                with m.Case(view):
                    m.d.comb += self.uo_out.eq(value)

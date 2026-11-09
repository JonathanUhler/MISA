from amaranth import Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .alu_unit import AluUnit
from .cmp_unit import CmpUnit
from .csr_reg_file import CsrRegFile
from .decoder_unit import DecoderUnit
from .gp_reg_file import GpRegFile


"""
States:
   
"""


class MisaCore(Component):

    mem_ready: In(1)
    mem_read_data: In(8)
    interrupt: In(1)

    mem_addr: Out(16)
    mem_write_data: Out(8)
    mem_write_enable: Out(1)
    mem_request: Out(1)
    halted: Out(1)
    halt_code: Out(8)


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_submodules(m)
        self.elaborate_datapath(m)

        return m


    def elaborate_submodules(self, m: Module) -> None:
        self.alu_unit = AluUnit()
        self.cmp_unit = CmpUnit()
        self.csr_reg_file = CsrRegFile()
        self.decoder_unit = DecoderUnit()
        self.gp_reg_file = GpRegFile()

        m.submodules.alu_unit = self.alu_unit
        m.submodules.cmp_unit = self.cmp_unit
        m.submodules.csr_reg_file = self.csr_reg_file
        m.submodules.decoder_unit = self.decoder_unit
        m.submodules.gp_reg_file = self.gp_reg_file


    def elaborate_datapath(self, m: Module) -> None:
        self.pc: Signal = Signal(16)
        self.inst: Signal = Signal(16)
        self.in_syscall: Signal = Signal(1)
        self.in_interrupt: Signal = Signal(1)
        self.int_rscratch: Signal = Signal(16)
        self.int_flags: Signal = Signal(4)
        self.syscall_privs: Signal = Signal(1)
        self.int_privs: Signal = Signal(1)

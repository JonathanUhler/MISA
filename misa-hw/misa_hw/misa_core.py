from amaranth import Module, Signal
from amaranth.lib.wiring import Component, In, Out

from .alu_unit import AluUnit
from .cmp_unit import CmpUnit
from .csr_reg_file import CsrRegFile
from .decoder_unit import DecoderUnit
from .gp_reg_file import GpRegFile


"""
MisaCore final FSM design

States are listed below. "Arguments" provided to states in function-like calls are assumed to be
kept as core-level/global values.

---

Mem handling:

MEM_REQUEST(type, addr, data, then)
  mem_addr <- addr
  If type is read
    mem_write_enable <- 0
  Else
    mem_write_data <- data
    mem_write_enable <- 1
  End if
  mem_request <- 1
  Next MEM_WAIT(type, data, then)

MEM_WAIT(type, data, then)
  If mem_ready
    If type is read
      data <- mem_read_data
    mem_request <- 0
    Next then
  Else
    Next MEM_WAIT

---

Global values (in addition to what each state operates on):

pc
inst

---

Main states:

VECTOR_LO(vec_base)
  Next MEM_REQUEST(read, vec_base, pc[0:8], LOAD_VECTOR_HI(vec_base))

VECTOR_HI(vec_base)
  Next MEM_REQUEST(read, vec_base + 1, pc[8:16], FETCH_LO)

FETCH_LO
  Next MEM_REQUEST(read, pc, inst[0:8], FETCH_HI)

FETCH_HI
  Next MEM_REQUEST(read, pc + 1, inst[8:16], DECODE)

DECODE
  If decoder.is_illegal
    Next FAULT(illegalInstruction)
  Elif privs = 1 and (inst.op = HALT or illegal RSR or illegal WSR)
    Next FAULT(privilegedInstruction)
  Elif inst.op = SYSCALL and (in syscall or in interrupt)
    Next FAULT(improperSystemCall)
  Elif (inst = JMP ALWAYS RETSC and (not in syscall or in interrupt)) or
       (inst = JMP ALWAYS RETIR and not in interrupt)
    Next FAULT(improperReturnContext)
  Elif (inst = JMP X RETSC or inst = JMP X RETIR) and X != ALWAYS
    Next FAULT(conditionalContextReturn)
  Else
    Switch inst.op
      Case ADD, ADC, SUB, SBB, AND, OR, XOR, RRC
        Next EXECUTE_ALU
      Case SET
        Next EXECUTE_SET
      Case LD
        Next EXECUTE_LD
      Case ST
        Next EXECUTE_ST
      Case RSR
        Next EXECUTE_RSR
      Case WSR
        Next EXECUTE_WSR
      Case JAL, JMP to GpReg
        Next EXECUTE_BRANCH
      Case JMP to CsrReg
        Next EXECUTE_JMP_CSR
      Case HALT
        Next EXECUTE_HALT
    

FAULT(cause)
"""


from enum import StrEnum
from typing import Callable

from amaranth.lib.enum import IntEnum


class CoreState(StrEnum):
    MEM_REQUEST = "MEM_REQUEST"
    MEM_WAIT    = "MEM_WAIT"

    VECTOR_LO = "VECTOR_LO"
    VECTOR_HI = "VECTOR_HI"

    FETCH_LO = "FETCH_LO"
    FETCH_HI = "FETCH_HI"

    DECODE = "DECODE"


class MemIssuer(IntEnum):
    VECTOR_LO = 0


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

        with m.FSM(init = CoreState.VECTOR_LO):
            self.elaborate_state(m, CoreState.MEM_REQUEST, mem_request)
            self.elaborate_state(m, CoreState.MEM_WAIT, mem_wait)

            self.elaborate_state(m, CoreState.VECTOR_LO, vector_lo)
            self.elaborate_state(m, CoreState.VECTOR_HI, vector_hi)

            self.elaborate_state(m, CoreState.FETCH_LO, fetch_lo)
            self.elaborate_state(m, CoreState.FETCH_HI, fetch_hi)

            self.elaborate_state(m, CoreState.DECODE, decode)

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
        # Main values (taken from misa-spec)
        self.pc: Signal = Signal(16)
        self.inst: Signal = Signal(16)
        self.in_syscall: Signal = Signal(1)
        self.in_interrupt: Signal = Signal(1)
        self.int_rscratch: Signal = Signal(16)
        self.int_flags: Signal = Signal(4)
        self.syscall_privs: Signal = Signal(1)
        self.int_privs: Signal = Signal(1)

        # Intermediate values (for states in the FSM to store persistent data)
        self.mem_issuer: Signal = Signal(MemIssuer)

        # Cross-module wiring


    def perform_mem_issue(self, m: Module, addr: Value, *, issuer: MemIssuer,
                          write_enable: Value = 0, write_data: Value = 0x00) -> None:
        m.d.sync += [
            self.mem_addr.eq(addr),
            self.mem_write_enable.eq(write_enable),
            self.mem_write_data.eq(write_data),
            self.mem_issuer.eq(issuer)
        ]
        m.next = CoreState.MEM_REQUEST


    def elaborate_state(self, m: Module, state: CoreState, action: Callable) -> None:
        with m.State(state):
            action(m)


    def mem_request(self, m: Module) -> None:
        m.d.comb += self.mem_request.eq(1)
        m.next = CoreState.MEM_WAIT


    def mem_wait(self, m: Module) -> None:
        with m.If(self.mem_ready):
            with m.Switch(self.mem_issuer):
                with m.Case(MemIssuer.VECTOR_LO):
                    m.d.sync += self.pc.eq(self.mem_read_data)
                    m.next = CoreState.VECTOR_HI


    def vector_lo(self, m: Module) -> None:
        self.perform_mem_issue(m, self.vec_base, issuer = MemIssuer.VECTOR_LO)


    def vector_hi(self, m: Module) -> None:
        self.perform_mem_issue(m, self.vec_base + 1, issuer = MemIssuer.VECTOR_HI)
            

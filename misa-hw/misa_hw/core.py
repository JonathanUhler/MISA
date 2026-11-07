from enum import StrEnum

from amaranth import C, Cat, Module, Signal, Value, ValueLike
from amaranth.lib.enum import IntEnum
from amaranth.lib.wiring import Component, In, Out

from .arithmetic import ArithmeticUnit
from .comparator import ComparatorUnit
from .csrfile import CsrFile
from .decoder import Decoder
from .grammar import Cmp, Csr, Extn, Flag, FaultKind, Op, Reg
from .init import CoreInit
from .regfile import RegFile


class CoreState(StrEnum):
    RESET0   = "RESET0"
    MEM_REQ  = "MEM_REQ"
    MEM_WAIT = "MEM_WAIT"

    FETCH0 = "FETCH0"
    EXEC   = "EXEC"

    ALU_R1 = "ALU_R1"
    ALU_R2 = "ALU_R2"
    ALU_WB = "ALU_WB"

    LD_R1   = "LD_R1"
    LD_R2   = "LD_R2"
    ST_R1   = "ST_R1"
    ST_R2   = "ST_R2"
    ST_R3   = "ST_R3"
    LDSTK_1 = "LDSTK_1"
    STSTK_1 = "STSTK_1"

    RSR_1 = "RSR_1"
    RSR_2 = "RSR_2"
    WSR_1 = "WSR_1"
    WSR_2 = "WSR_2"

    JAL_R1   = "JAL_R1"
    JAL_R2   = "JAL_R2"
    JMP_R1   = "JMP_R1"
    JMP_R2   = "JMP_R2"
    JMPCSR_1 = "JMPCSR_1"

    FAULT_1 = "FAULT_1"
    FAULT_2 = "FAULT_2"

    SYSCALL_1 = "SYSCALL_1"
    SYSCALL_2 = "SYSCALL_2"
    SYSCALL_3 = "SYSCALL_3"

    INTERRUPT_1  = "INTERRUPT_1"
    INTERRUPT_2  = "INTERRUPT_2"
    INTERRUPT_3  = "INTERRUPT_3"
    RETI_1 = "RETI_1"
    RETI_2 = "RETI_2"

    HALTED = "HALTED"


class MemReturn(IntEnum, shape = 4):
    RESET_LO     = 0
    RESET_HI     = 1
    FETCH_LO     = 2
    FETCH_HI     = 3
    LD_DONE      = 4
    ST_DONE      = 5
    FAULT_LO     = 6
    FAULT_HI     = 7
    SYSCALL_LO   = 8
    SYSCALL_HI   = 9
    INTERRUPT_LO = 10
    INTERRUPT_HI = 11


class Core(Component):

    mem_ready: In(1)
    mem_read_data: In(8)
    interrupt: In(1)

    mem_addr: Out(16)
    mem_write_data: Out(8)
    mem_write_enable: Out(1)
    mem_request: Out(1)
    halted: Out(1)
    halt_code: Out(8)
    debug_fetch: Out(1)


    def __init__(self, init_data: CoreInit = None) -> None:
        self.init_data: CoreInit = init_data or CoreInit()
        self.init_state: CoreState = \
            CoreState.RESET0  if init_data is None else \
            (CoreState.HALTED if init_data.halted  else CoreState.FETCH0)
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_datapath(m)
        self.elaborate_interrupt_latch(m)

        with m.FSM(init = self.init_state):
            self.elaborate_memory_states(m)
            self.elaborate_fetch_execute_states(m)
            self.elaborate_alu_states(m)
            self.elaborate_load_store_states(m)
            self.elaborate_csr_states(m)
            self.elaborate_branch_states(m)
            self.elaborate_fault_states(m)
            self.elaborate_syscall_states(m)
            self.elaborate_interrupt_states(m)
            with m.State(CoreState.HALTED):
                m.next = CoreState.HALTED

        return m


    def elaborate_datapath(self, m: Module) -> None:
        self.regfile: RegFile = RegFile(init_data = self.init_data.reg_init)
        self.csrfile: CsrFile = CsrFile(init_data = self.init_data.csr_init)
        self.decoder: Decoder = Decoder()
        self.arithmetic_unit: ArithmeticUnit = ArithmeticUnit()
        self.comparator_unit: ComparatorUnit = ComparatorUnit()

        m.submodules.regfile = self.regfile
        m.submodules.csrfile = self.csrfile
        m.submodules.decoder = self.decoder
        m.submodules.arithmetic_unit = self.arithmetic_unit
        m.submodules.comparator_unit = self.comparator_unit

        self.pc: Signal = Signal(16, init = self.init_data.pc)
        self.pc_lo: Signal = Signal(8)
        self.inst_lo: Signal = Signal(8)
        self.inst_hi: Signal = Signal(8)
        self.mem_op_addr: Signal = Signal(16)
        self.mem_op_write_enable: Signal = Signal(1)
        self.mem_op_write_data: Signal = Signal(8)
        self.reg_a_data: Signal = Signal(8)
        self.reg_b_data: Signal = Signal(8)
        self.base_hi: Signal = Signal(8)
        self.effective_addr: Signal = Signal(16)
        self.rsr_lo: Signal = Signal(8)
        self.cause_data: Signal = Signal(16)
        self.in_syscall: Signal = Signal(1, init = self.init_data.in_syscall)
        self.in_interrupt: Signal = Signal(1, init = self.init_data.in_interrupt)
        self.syscall_privs: Signal = Signal(1, init = self.init_data.syscall_privs & 1)
        self.int_rscratch: Signal = Signal(16, init = self.init_data.int_rscratch)
        self.int_rscratch_hi: Signal = Signal(8)
        self.int_flags: Signal = Signal(4, init = self.init_data.int_flags & 0xF)
        self.int_privs: Signal = Signal(1, init = self.init_data.int_privs & 1)
        self.mem_return: Signal = Signal(MemReturn)
        self.interrupt_pending: Signal = Signal(1)

        m.d.comb += self.decoder.inst.eq(Cat(self.inst_lo, self.inst_hi))

        self.imm_sext: Value = Cat(self.decoder.out.imm4, self.decoder.out.imm4[3].replicate(12))
        self.low_idx: Value = (self.decoder.out.n2 + 1)[0 : 4]
        self.ext_syscall: Value = (self.csrfile.extns_data & Extn.SYSCALL).any()
        self.ext_interrupt: Value = (self.csrfile.extns_data & Extn.INTERRUPT).any()
        self.ext_privilege: Value = (self.csrfile.extns_data & Extn.PRIVILEGE).any()

        m.d.comb += [
            self.mem_addr.eq(self.mem_op_addr),
            self.mem_write_enable.eq(self.mem_op_write_enable),
            self.mem_write_data.eq(self.mem_op_write_data),
            self.mem_request.eq(0),
            self.regfile.read_reg.eq(0),
            self.regfile.write_reg.eq(0),
            self.regfile.write_data.eq(0),
            self.regfile.write_enable.eq(0),
            self.csrfile.read_reg.eq(Csr.SADDR),
            self.csrfile.write_reg.eq(Csr.SADDR),
            self.csrfile.write_data.eq(0),
            self.csrfile.write_enable.eq(0),
            self.csrfile.flags_write_enable.eq(0),
            self.csrfile.flags_write_data.eq(0),
            self.arithmetic_unit.op.eq(self.decoder.out.op),
            self.arithmetic_unit.a.eq(self.reg_a_data),
            self.arithmetic_unit.b.eq(self.reg_b_data),
            self.arithmetic_unit.cin.eq((self.csrfile.flags_data & Flag.C).any()),
            self.comparator_unit.z.eq((self.csrfile.flags_data & Flag.Z).any()),
            self.comparator_unit.c.eq((self.csrfile.flags_data & Flag.C).any()),
            self.comparator_unit.n.eq((self.csrfile.flags_data & Flag.N).any()),
            self.comparator_unit.v.eq((self.csrfile.flags_data & Flag.V).any()),
            self.comparator_unit.cmp.eq(self.decoder.out.cmp),
            self.debug_fetch.eq(0)
        ]


    def elaborate_interrupt_latch(self, m: Module) -> None:
        interrupt_sync0: Signal = Signal(1)
        interrupt_sync1: Signal = Signal(1)
        interrupt_prev: Signal = Signal(1)
        m.d.sync += [
            interrupt_sync0.eq(self.interrupt),
            interrupt_sync1.eq(interrupt_sync0),
            interrupt_prev.eq(interrupt_sync1)
        ]
        with m.If(interrupt_sync1 & ~interrupt_prev):
            m.d.sync += self.interrupt_pending.eq(1)


    def issue(self, m: Module, addr: ValueLike, we: ValueLike = 0,
              wdata: ValueLike = 0, then: MemReturn = None) -> None:
        m.d.sync += [
            self.mem_op_addr.eq(addr),
            self.mem_op_write_enable.eq(we),
            self.mem_op_write_data.eq(wdata)
        ]
        if then is not None:
            m.d.sync += self.mem_return.eq(then)
        m.next = CoreState.MEM_REQ


    def do_fault(self, m: Module, kind: int, status: ValueLike = None) -> None:
        fault_status: ValueLike = C(0, 8) if status is None else status
        m.d.sync += self.cause_data.eq(Cat(C(kind, 6), C(0, 2), fault_status))
        m.next = CoreState.FAULT_1


    def advance(self, m: Module) -> None:
        with m.If((self.pc + 2)[16]):
            self.do_fault(m, int(FaultKind.PC_OVERFLOW))
        with m.Else():
            m.d.sync += self.pc.eq(self.pc + 2)
            m.next = CoreState.FETCH0


    def elaborate_memory_states(self, m: Module) -> None:
        with m.State(CoreState.RESET0):
            self.issue(m, 0xFFFE, then = MemReturn.RESET_LO)

        with m.State(CoreState.MEM_REQ):
            m.d.comb += self.mem_request.eq(1)
            m.next = CoreState.MEM_WAIT

        with m.State(CoreState.MEM_WAIT):
            with m.If(self.mem_ready):
                with m.Switch(self.mem_return):
                    with m.Case(MemReturn.RESET_LO):
                        m.d.sync += self.pc_lo.eq(self.mem_read_data)
                        self.issue(m, 0xFFFF, then = MemReturn.RESET_HI)
                    with m.Case(MemReturn.RESET_HI):
                        m.d.sync += self.pc.eq(Cat(self.pc_lo, self.mem_read_data))
                        m.next = CoreState.FETCH0
                    with m.Case(MemReturn.FETCH_LO):
                        m.d.sync += self.inst_lo.eq(self.mem_read_data)
                        self.issue(m, self.pc + 1, then = MemReturn.FETCH_HI)
                    with m.Case(MemReturn.FETCH_HI):
                        m.d.sync += self.inst_hi.eq(self.mem_read_data)
                        m.next = CoreState.EXEC
                    with m.Case(MemReturn.LD_DONE):
                        m.d.comb += [
                            self.regfile.write_reg.eq(self.decoder.out.n1),
                            self.regfile.write_data.eq(self.mem_read_data),
                            self.regfile.write_enable.eq(1)
                        ]
                        self.advance(m)
                    with m.Case(MemReturn.ST_DONE):
                        self.advance(m)
                    with m.Case(MemReturn.FAULT_LO):
                        m.d.sync += self.pc_lo.eq(self.mem_read_data)
                        self.issue(m, 0xFFFD, then = MemReturn.FAULT_HI)
                    with m.Case(MemReturn.FAULT_HI):
                        m.d.sync += self.pc.eq(Cat(self.pc_lo, self.mem_read_data))
                        m.next = CoreState.FETCH0
                    with m.Case(MemReturn.SYSCALL_LO):
                        m.d.sync += self.pc_lo.eq(self.mem_read_data)
                        self.issue(m, 0xFFFB, then = MemReturn.SYSCALL_HI)
                    with m.Case(MemReturn.SYSCALL_HI):
                        m.d.sync += self.pc.eq(Cat(self.pc_lo, self.mem_read_data))
                        m.next = CoreState.FETCH0
                    with m.Case(MemReturn.INTERRUPT_LO):
                        m.d.sync += self.pc_lo.eq(self.mem_read_data)
                        self.issue(m, 0xFFF9, then = MemReturn.INTERRUPT_HI)
                    with m.Case(MemReturn.INTERRUPT_HI):
                        m.d.sync += self.pc.eq(Cat(self.pc_lo, self.mem_read_data))
                        m.next = CoreState.FETCH0


    def elaborate_fetch_execute_states(self, m: Module) -> None:
        with m.State(CoreState.FETCH0):
            m.d.comb += self.debug_fetch.eq(1)
            can_int: Value = self.ext_interrupt & ~self.in_interrupt & self.interrupt_pending
            with m.If(can_int):
                m.next = CoreState.INTERRUPT_1
            with m.Else():
                self.issue(m, self.pc, then = MemReturn.FETCH_LO)

        with m.State(CoreState.EXEC):
            sysc: Value = self.ext_syscall
            intr: Value = self.ext_interrupt
            priv: Value = self.ext_privilege

            def unavail(csr_field: Value) -> Value:
                return (((csr_field == Csr.RETSC) & ~sysc)
                        | ((csr_field == Csr.RETIR) & ~intr)
                        | ((csr_field == Csr.PRIVS) & ~priv))

            is_rsr_wsr: Value = (self.decoder.out.op == Op.RSR) | (self.decoder.out.op == Op.WSR)
            is_jmpcsr: Value = (self.decoder.out.op == Op.JMP) & self.decoder.out.csr_target
            csr_bad: Value =                                  \
                (is_rsr_wsr & unavail(self.decoder.out.n3)) | \
                (is_jmpcsr & unavail(self.decoder.out.n1))

            user_mode: Value = priv & self.csrfile.privs_data

            def user_readable(csr_field: Value) -> Value:
                return ((csr_field == Csr.SADDR) | (csr_field == Csr.RADDR)
                        | (csr_field == Csr.FLAGS) | (csr_field == Csr.PRIVS))

            def user_writable(csr_field: Value) -> Value:
                return ((csr_field == Csr.SADDR) | (csr_field == Csr.RADDR)
                        | (csr_field == Csr.FLAGS))

            with m.If(self.decoder.out.illegal | csr_bad):
                self.do_fault(m, int(FaultKind.ILLEGAL))
            with m.Else():
                with m.Switch(self.decoder.out.op):
                    with m.Case(Op.SET):
                        m.d.comb += [
                            self.regfile.write_reg.eq(self.decoder.out.n1),
                            self.regfile.write_data.eq(self.decoder.out.imm8),
                            self.regfile.write_enable.eq(1)
                        ]
                        self.advance(m)
                    with m.Case(Op.HALT):
                        with m.If(self.decoder.out.is_syscall & sysc):
                            with m.If(self.in_syscall | self.in_interrupt):
                                self.do_fault(
                                    m, int(FaultKind.IMPROPER_SYSCALL),
                                    status = Cat(self.in_syscall, self.in_interrupt, C(0, 6))
                                )
                            with m.Else():
                                m.next = CoreState.SYSCALL_1
                        with m.Elif(user_mode):
                            self.do_fault(m, int(FaultKind.PRIV_INSTRUCTION))
                        with m.Else():
                            m.d.comb += [
                                self.regfile.read_reg.eq(self.decoder.out.n1),
                                self.csrfile.write_reg.eq(Csr.CAUSE),
                                self.csrfile.write_data.eq(Cat(C(0x01, 8), self.regfile.read_data)),
                                self.csrfile.write_enable.eq(1)
                            ]
                            m.d.sync += [
                                self.halted.eq(1), self.halt_code.eq(self.regfile.read_data)
                            ]
                            m.next = CoreState.HALTED
                    with m.Case(Op.ADD, Op.ADC, Op.SUB, Op.SBB, Op.AND, Op.OR, Op.XOR, Op.RRC):
                        m.next = CoreState.ALU_R1
                    with m.Case(Op.LD):
                        with m.If(self.decoder.out.n2 == 0xF):
                            m.next = CoreState.LDSTK_1
                        with m.Else():
                            m.next = CoreState.LD_R1
                    with m.Case(Op.ST):
                        with m.If(self.decoder.out.n2 == 0xF):
                            m.next = CoreState.STSTK_1
                        with m.Else():
                            m.next = CoreState.ST_R1
                    with m.Case(Op.RSR):
                        with m.If(user_mode & ~user_readable(self.decoder.out.n3)):
                            self.do_fault(m, int(FaultKind.PRIV_INSTRUCTION))
                        with m.Else():
                            m.next = CoreState.RSR_1
                    with m.Case(Op.WSR):
                        with m.If(user_mode & ~user_writable(self.decoder.out.n3)):
                            self.do_fault(m, int(FaultKind.PRIV_INSTRUCTION))
                        with m.Else():
                            m.next = CoreState.WSR_1
                    with m.Case(Op.JAL):
                        m.next = CoreState.JAL_R1
                    with m.Case(Op.JMP):
                        with m.If(self.decoder.out.csr_target):
                            m.next = CoreState.JMPCSR_1
                        with m.Else():
                            m.next = CoreState.JMP_R1
                    with m.Default():
                        self.advance(m)


    def elaborate_alu_states(self, m: Module) -> None:

        with m.State(CoreState.ALU_R1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n2)
            m.d.sync += self.reg_a_data.eq(self.regfile.read_data)
            m.next = CoreState.ALU_R2

        with m.State(CoreState.ALU_R2):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n3)
            m.d.sync += self.reg_b_data.eq(self.regfile.read_data)
            m.next = CoreState.ALU_WB

        with m.State(CoreState.ALU_WB):
            m.d.comb += [
                self.regfile.write_reg.eq(self.decoder.out.n1),
                self.regfile.write_data.eq(self.arithmetic_unit.y),
                self.regfile.write_enable.eq(1),
                self.csrfile.flags_write_enable.eq(1),
                self.csrfile.flags_write_data.eq(Cat(self.arithmetic_unit.z,
                                                     self.arithmetic_unit.c,
                                                     self.arithmetic_unit.n,
                                                     self.arithmetic_unit.v))
            ]
            self.advance(m)


    def elaborate_load_store_states(self, m: Module) -> None:

        with m.State(CoreState.LD_R1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n2)
            m.d.sync += self.base_hi.eq(self.regfile.read_data)
            m.next = CoreState.LD_R2

        with m.State(CoreState.LD_R2):
            m.d.comb += self.regfile.read_reg.eq(self.low_idx)
            self.issue(m, Cat(self.regfile.read_data, self.base_hi) + self.imm_sext,
                       then = MemReturn.LD_DONE)

        with m.State(CoreState.ST_R1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n2)
            m.d.sync += self.base_hi.eq(self.regfile.read_data)
            m.next = CoreState.ST_R2

        with m.State(CoreState.ST_R2):
            m.d.comb += self.regfile.read_reg.eq(self.low_idx)
            m.d.sync += \
                self.effective_addr.eq(Cat(self.regfile.read_data, self.base_hi) + self.imm_sext)
            m.next = CoreState.ST_R3

        with m.State(CoreState.ST_R3):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n1)
            self.issue(m, self.effective_addr, we = 1, wdata = self.regfile.read_data,
                       then = MemReturn.ST_DONE)

        with m.State(CoreState.LDSTK_1):
            m.d.comb += self.csrfile.read_reg.eq(Csr.SADDR)
            new_saddr: Value = self.csrfile.read_data + self.imm_sext
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.SADDR),
                self.csrfile.write_data.eq(new_saddr),
                self.csrfile.write_enable.eq(1)
            ]
            self.issue(m, new_saddr, then = MemReturn.LD_DONE)

        with m.State(CoreState.STSTK_1):
            m.d.comb += [
                self.csrfile.read_reg.eq(Csr.SADDR), self.regfile.read_reg.eq(self.decoder.out.n1)
            ]
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.SADDR),
                self.csrfile.write_data.eq(self.csrfile.read_data + self.imm_sext),
                self.csrfile.write_enable.eq(1)
            ]
            self.issue(m, self.csrfile.read_data, we = 1, wdata = self.regfile.read_data,
                       then = MemReturn.ST_DONE)


    def elaborate_csr_states(self, m: Module) -> None:

        with m.State(CoreState.RSR_1):
            m.d.comb += [
                self.csrfile.read_reg.eq(self.decoder.out.n3),
                self.regfile.write_reg.eq(self.decoder.out.n1),
                self.regfile.write_data.eq(self.csrfile.read_data[8 : 16]),
                self.regfile.write_enable.eq(1)
            ]
            m.d.sync += self.rsr_lo.eq(self.csrfile.read_data[0 : 8])
            m.next = CoreState.RSR_2

        with m.State(CoreState.RSR_2):
            m.d.comb += [
                self.regfile.write_reg.eq(self.decoder.out.n2),
                self.regfile.write_data.eq(self.rsr_lo),
                self.regfile.write_enable.eq(1)
            ]
            self.advance(m)

        with m.State(CoreState.WSR_1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n1)
            m.d.sync += self.base_hi.eq(self.regfile.read_data)
            m.next = CoreState.WSR_2

        with m.State(CoreState.WSR_2):
            m.d.comb += [
                self.regfile.read_reg.eq(self.decoder.out.n2),
                self.csrfile.write_reg.eq(self.decoder.out.n3),
                self.csrfile.write_data.eq(Cat(self.regfile.read_data, self.base_hi)),
                self.csrfile.write_enable.eq(1)
            ]
            self.advance(m)


    def elaborate_branch_states(self, m: Module) -> None:

        with m.State(CoreState.JAL_R1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n1)
            m.d.sync += self.base_hi.eq(self.regfile.read_data)
            m.next = CoreState.JAL_R2

        with m.State(CoreState.JAL_R2):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n2)
            with m.If(self.comparator_unit.taken):
                m.d.comb += [
                    self.csrfile.write_reg.eq(Csr.RADDR),
                    self.csrfile.write_data.eq(self.pc + 2),
                    self.csrfile.write_enable.eq(1)
                ]
                m.d.sync += self.pc.eq(Cat(self.regfile.read_data, self.base_hi))
                m.next = CoreState.FETCH0
            with m.Else():
                self.advance(m)

        with m.State(CoreState.JMP_R1):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n1)
            m.d.sync += self.base_hi.eq(self.regfile.read_data)
            m.next = CoreState.JMP_R2

        with m.State(CoreState.JMP_R2):
            m.d.comb += self.regfile.read_reg.eq(self.decoder.out.n2)
            with m.If(self.comparator_unit.taken):
                m.d.sync += self.pc.eq(Cat(self.regfile.read_data, self.base_hi))
                m.next = CoreState.FETCH0
            with m.Else():
                self.advance(m)

        with m.State(CoreState.JMPCSR_1):
            m.d.comb += self.csrfile.read_reg.eq(self.decoder.out.n1)
            with m.If(self.comparator_unit.taken):
                with m.If(self.decoder.out.n1 == Csr.RETSC):
                    with m.If(
                        (self.decoder.out.cmp == Cmp.ALWAYS) & self.in_syscall & ~self.in_interrupt
                    ):
                        m.d.comb += [
                            self.csrfile.write_reg.eq(Csr.PRIVS),
                            self.csrfile.write_data.eq(self.syscall_privs),
                            self.csrfile.write_enable.eq(1)
                        ]
                        m.d.sync += [self.pc.eq(self.csrfile.read_data), self.in_syscall.eq(0)]
                        m.next = CoreState.FETCH0
                    with m.Elif(self.decoder.out.cmp == Cmp.ALWAYS):
                        self.do_fault(
                            m, int(FaultKind.IMPROPER_RETURN),
                            status = Cat(self.in_syscall, self.in_interrupt,
                                         C(0, 2), C(int(Csr.RETSC), 4))
                        )
                    with m.Else():
                        self.do_fault(
                            m, int(FaultKind.COND_RETURN),
                            status = Cat(self.decoder.out.cmp, C(0, 1), C(int(Csr.RETSC), 4))
                        )
                with m.Elif(self.decoder.out.n1 == Csr.RETIR):
                    with m.If((self.decoder.out.cmp == Cmp.ALWAYS) & self.in_interrupt):
                        m.next = CoreState.RETI_1
                    with m.Elif(self.decoder.out.cmp == Cmp.ALWAYS):
                        self.do_fault(
                            m, int(FaultKind.IMPROPER_RETURN),
                            status = Cat(self.in_syscall, self.in_interrupt,
                                         C(0, 2), C(int(Csr.RETIR), 4))
                        )
                    with m.Else():
                        self.do_fault(
                            m, int(FaultKind.COND_RETURN),
                            status = Cat(self.decoder.out.cmp, C(0, 1), C(int(Csr.RETIR), 4))
                        )
                with m.Else():
                    m.d.sync += self.pc.eq(self.csrfile.read_data)
                    m.next = CoreState.FETCH0
            with m.Else():
                self.advance(m)


    def elaborate_fault_states(self, m: Module) -> None:

        with m.State(CoreState.FAULT_1):
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.CAUSE),
                self.csrfile.write_data.eq(self.cause_data),
                self.csrfile.write_enable.eq(1)
            ]
            m.next = CoreState.FAULT_2

        with m.State(CoreState.FAULT_2):
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.PRIVS),
                self.csrfile.write_data.eq(0),
                self.csrfile.write_enable.eq(1)
            ]
            self.issue(m, 0xFFFC, then = MemReturn.FAULT_LO)


    def elaborate_syscall_states(self, m: Module) -> None:

        with m.State(CoreState.SYSCALL_1):
            m.d.comb += [
                self.regfile.read_reg.eq(self.decoder.out.n1),
                self.csrfile.write_reg.eq(Csr.CAUSE),
                self.csrfile.write_data.eq(Cat(C(0x01, 8), self.regfile.read_data)),
                self.csrfile.write_enable.eq(1)
            ]
            m.next = CoreState.SYSCALL_2

        with m.State(CoreState.SYSCALL_2):
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.RETSC),
                self.csrfile.write_data.eq(self.pc + 2),
                self.csrfile.write_enable.eq(1)
            ]
            m.next = CoreState.SYSCALL_3

        with m.State(CoreState.SYSCALL_3):
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.PRIVS),
                self.csrfile.write_data.eq(0),
                self.csrfile.write_enable.eq(1)
            ]
            m.d.sync += [self.syscall_privs.eq(self.csrfile.privs_data), self.in_syscall.eq(1)]
            self.issue(m, 0xFFFA, then = MemReturn.SYSCALL_LO)


    def elaborate_interrupt_states(self, m: Module) -> None:

        with m.State(CoreState.INTERRUPT_1):
            m.d.comb += self.regfile.read_reg.eq(Reg.RSCRATCH0)
            m.d.sync += self.int_rscratch_hi.eq(self.regfile.read_data)
            m.next = CoreState.INTERRUPT_2

        with m.State(CoreState.INTERRUPT_2):
            m.d.comb += [
                self.regfile.read_reg.eq(Reg.RSCRATCH1),
                self.csrfile.write_reg.eq(Csr.RETIR),
                self.csrfile.write_data.eq(self.pc),
                self.csrfile.write_enable.eq(1)
            ]
            m.d.sync += [
                self.int_rscratch.eq(Cat(self.regfile.read_data, self.int_rscratch_hi)),
                self.int_flags.eq(self.csrfile.flags_data),
                self.int_privs.eq(self.csrfile.privs_data),
                self.in_interrupt.eq(1),
                self.interrupt_pending.eq(0)
            ]
            m.next = CoreState.INTERRUPT_3

        with m.State(CoreState.INTERRUPT_3):
            m.d.comb += [
                self.csrfile.write_reg.eq(Csr.PRIVS),
                self.csrfile.write_data.eq(0),
                self.csrfile.write_enable.eq(1)
            ]
            self.issue(m, 0xFFF8, then = MemReturn.INTERRUPT_LO)

        with m.State(CoreState.RETI_1):
            m.d.comb += [
                self.regfile.write_reg.eq(Reg.RSCRATCH0),
                self.regfile.write_data.eq(self.int_rscratch[8 : 16]),
                self.regfile.write_enable.eq(1),
                self.csrfile.flags_write_enable.eq(1),
                self.csrfile.flags_write_data.eq(self.int_flags)
            ]
            m.next = CoreState.RETI_2

        with m.State(CoreState.RETI_2):
            m.d.comb += [
                self.regfile.write_reg.eq(Reg.RSCRATCH1),
                self.regfile.write_data.eq(self.int_rscratch[0 : 8]),
                self.regfile.write_enable.eq(1),
                self.csrfile.read_reg.eq(Csr.RETIR),
                self.csrfile.write_reg.eq(Csr.PRIVS),
                self.csrfile.write_data.eq(self.int_privs),
                self.csrfile.write_enable.eq(1)
            ]
            m.d.sync += [self.pc.eq(self.csrfile.read_data), self.in_interrupt.eq(0)]
            m.next = CoreState.FETCH0

from dataclasses import replace
from enum import StrEnum

from amaranth import C, Cat, Module, Mux, Signal, Value
from amaranth.lib.enum import IntEnum
from amaranth.lib.wiring import Component, In, Out

from .alu_unit import AluUnit
from .cmp_unit import CmpUnit
from .csr_reg_file import CsrRegFile
from .decoder_unit import DecoderUnit
from .gp_reg_file import GpRegFile
from .grammar import AccessKind, CmpFlag, CsrReg, Extn, FaultKind, GpReg, OverflowKind, Op
from .init import CoreInit


class VectorSelect(IntEnum, shape = 2):
    INTERRUPT = 0b00
    SYSCALL   = 0b01
    FAULT     = 0b10
    RESET     = 0b11


class CoreState(StrEnum):
    VECTOR_LO = "VECTOR_LO"
    VECTOR_HI = "VECTOR_HI"

    FETCH_LO = "FETCH_LO"
    FETCH_HI = "FETCH_HI"
    DECODE   = "DECODE"

    EXECUTE_ALU      = "EXECUTE_ALU"
    EXECUTE_SET      = "EXECUTE_SET"
    EXECUTE_LD       = "EXECUTE_LD"
    EXECUTE_LD_STK   = "EXECUTE_LD_STK"
    EXECUTE_ST_DATA  = "EXECUTE_ST_DATA"
    EXECUTE_ST       = "EXECUTE_ST"
    EXECUTE_ST_STK   = "EXECUTE_ST_STK"
    EXECUTE_RSR_HI   = "EXECUTE_RSR_HI"
    EXECUTE_RSR_LO   = "EXECUTE_RSR_LO"
    EXECUTE_WSR      = "EXECUTE_WSR"
    EXECUTE_BRANCH   = "EXECUTE_BRANCH"
    EXECUTE_JMP_CSR  = "EXECUTE_JMP_CSR"
    EXECUTE_RETIR_HI = "EXECUTE_RETIR_HI"
    EXECUTE_RETIR_LO = "EXECUTE_RETIR_LO"
    EXECUTE_HALT     = "EXECUTE_HALT"
    HALTED           = "HALTED"

    TRAP_CAUSE      = "TRAP_CAUSE"
    TRAP_LINK       = "TRAP_LINK"
    TRAP_ENTER      = "TRAP_ENTER"
    TRAP_HALT_CAUSE = "TRAP_HALT_CAUSE"
    TRAP_HALT_FAULT = "TRAP_HALT_FAULT"


CORE_STATE_CODES: dict = {
    CoreState.VECTOR_LO:         0,
    CoreState.VECTOR_HI:         1,
    CoreState.FETCH_LO:          2,
    CoreState.FETCH_HI:          3,
    CoreState.DECODE:            4,
    CoreState.EXECUTE_ALU:       5,
    CoreState.EXECUTE_SET:       6,
    CoreState.EXECUTE_LD:        7,
    CoreState.EXECUTE_LD_STK:    8,
    CoreState.EXECUTE_ST_DATA:   9,
    CoreState.EXECUTE_ST:       10,
    CoreState.EXECUTE_ST_STK:   11,
    CoreState.EXECUTE_RSR_HI:   12,
    CoreState.EXECUTE_RSR_LO:   13,
    CoreState.EXECUTE_WSR:      14,
    CoreState.EXECUTE_BRANCH:   15,
    CoreState.EXECUTE_JMP_CSR:  16,
    CoreState.EXECUTE_RETIR_HI: 17,
    CoreState.EXECUTE_RETIR_LO: 18,
    CoreState.EXECUTE_HALT:     19,
    CoreState.HALTED:           20,
    CoreState.TRAP_CAUSE:       21,
    CoreState.TRAP_LINK:        22,
    CoreState.TRAP_ENTER:       23,
    CoreState.TRAP_HALT_CAUSE:  24,
    CoreState.TRAP_HALT_FAULT:  25
}

CORE_STATE_BITS: int = (len(CORE_STATE_CODES) - 1).bit_length()


class MisaCore(Component):

    mem_ready: In(1)
    mem_read_data: In(8)
    mem_mapped: In(1)
    mem_caps: In(3)
    interrupt: In(1)

    mem_addr: Out(16)
    mem_write_data: Out(8)
    mem_write_enable: Out(1)
    mem_request: Out(1)
    cause_data: Out(16)
    fault_data: Out(16)
    pc_data: Out(16)
    boundary_data: Out(1)
    state_data: Out(CORE_STATE_BITS)
    in_syscall_data: Out(1)
    in_interrupt_data: Out(1)
    int_pending_data: Out(1)
    user_mode_data: Out(1)


    def __init__(self, init_data: CoreInit = None, reset_extns: int = None):
        self.init_data: CoreInit = init_data or CoreInit()
        if (reset_extns is not None):
            self.init_data = replace(
                self.init_data, special = replace(self.init_data.special, extns = reset_extns)
            )
        self.init_state: CoreState = CoreState.VECTOR_LO
        if (init_data is not None):
            self.init_state = CoreState.HALTED if init_data.halted else CoreState.FETCH_LO
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_submodules(m)
        self.elaborate_datapath(m)

        states: list = [
            (CoreState.VECTOR_LO,        self.elaborate_vector_lo),
            (CoreState.VECTOR_HI,        self.elaborate_vector_hi),
            (CoreState.FETCH_LO,         self.elaborate_fetch_lo),
            (CoreState.FETCH_HI,         self.elaborate_fetch_hi),
            (CoreState.DECODE,           self.elaborate_decode),
            (CoreState.EXECUTE_ALU,      self.elaborate_execute_alu),
            (CoreState.EXECUTE_SET,      self.elaborate_execute_set),
            (CoreState.EXECUTE_LD,       self.elaborate_execute_ld),
            (CoreState.EXECUTE_LD_STK,   self.elaborate_execute_ld_stk),
            (CoreState.EXECUTE_ST_DATA,  self.elaborate_execute_st_data),
            (CoreState.EXECUTE_ST,       self.elaborate_execute_st),
            (CoreState.EXECUTE_ST_STK,   self.elaborate_execute_st_stk),
            (CoreState.EXECUTE_RSR_HI,   self.elaborate_execute_rsr_hi),
            (CoreState.EXECUTE_RSR_LO,   self.elaborate_execute_rsr_lo),
            (CoreState.EXECUTE_WSR,      self.elaborate_execute_wsr),
            (CoreState.EXECUTE_BRANCH,   self.elaborate_execute_branch),
            (CoreState.EXECUTE_JMP_CSR,  self.elaborate_execute_jmp_csr),
            (CoreState.EXECUTE_RETIR_HI, self.elaborate_execute_retir_hi),
            (CoreState.EXECUTE_RETIR_LO, self.elaborate_execute_retir_lo),
            (CoreState.EXECUTE_HALT,     self.elaborate_execute_halt),
            (CoreState.HALTED,           self.elaborate_halted),
            (CoreState.TRAP_CAUSE,       self.elaborate_trap_cause),
            (CoreState.TRAP_LINK,        self.elaborate_trap_link),
            (CoreState.TRAP_ENTER,       self.elaborate_trap_enter),
            (CoreState.TRAP_HALT_CAUSE,  self.elaborate_trap_halt_cause),
            (CoreState.TRAP_HALT_FAULT,  self.elaborate_trap_halt_fault)
        ]

        with m.FSM(init = self.init_state) as fsm:
            fsm.encoding.update(CORE_STATE_CODES)
            for state, elaborator in states:
                with m.State(state):
                    elaborator(m)

        assert fsm.encoding.keys() == CORE_STATE_CODES.keys(), \
            "MisaCore: CORE_STATE_CODES does not enumerate every state of the FSM"
        m.d.comb += self.state_data.eq(fsm.state)

        return m


    def elaborate_submodules(self, m: Module) -> None:
        self.alu_unit = AluUnit()
        self.cmp_unit = CmpUnit()
        self.csr_reg_file = CsrRegFile(self.init_data.special, self.init_data.mpu)
        self.decoder_unit = DecoderUnit()
        self.gp_reg_file = GpRegFile(self.init_data.register)

        m.submodules.alu_unit = self.alu_unit
        m.submodules.cmp_unit = self.cmp_unit
        m.submodules.csr_reg_file = self.csr_reg_file
        m.submodules.decoder_unit = self.decoder_unit
        m.submodules.gp_reg_file = self.gp_reg_file


    def elaborate_datapath(self, m: Module) -> None:
        init: CoreInit = self.init_data

        # Main values (taken from misa-spec)
        self.pc: Signal = Signal(16, init = init.pc)
        self.inst: Signal = Signal(16)
        self.in_syscall: Signal = Signal(1, init = init.in_syscall)
        self.in_interrupt: Signal = Signal(1, init = init.in_interrupt)
        self.int_pending: Signal = Signal(1, init = init.int_pending)
        self.int_rscratch: Signal = Signal(16, init = init.int_rscratch)
        self.int_flags: Signal = Signal(4, init = init.int_flags & 0x000F)
        self.syscall_privs: Signal = Signal(1, init = init.syscall_privs & 0x0001)
        self.int_privs: Signal = Signal(1, init = init.int_privs & 0x0001)

        # Intermediate values (for states in the FSM to store persistent data)
        self.cause: Signal = Signal(16)
        self.fault_loc: Signal = Signal(16)
        self.vector_sel: Signal = Signal(VectorSelect, init = VectorSelect.RESET)
        self.data_buf: Signal = Signal(8)

        # Submodule wiring. The ALU operands and the comparison inputs are static, because the only
        # states which consume either result are the ones which drive the register file ports.
        m.d.comb += [
            self.decoder_unit.inst.eq(self.inst),
            self.decoder_unit.extns.eq(self.csr_reg_file.extns_data),

            self.alu_unit.alu_op.eq(self.decoder_unit.op),
            self.alu_unit.a.eq(self.gp_reg_file.read_data_a),
            self.alu_unit.b.eq(self.gp_reg_file.read_data_b),
            self.alu_unit.cin.eq(self.csr_reg_file.flags_data[1]),

            self.cmp_unit.cmp_flag.eq(self.decoder_unit.cmp),
            self.cmp_unit.z.eq(self.csr_reg_file.flags_data[0]),
            self.cmp_unit.c.eq(self.csr_reg_file.flags_data[1]),
            self.cmp_unit.n.eq(self.csr_reg_file.flags_data[2]),
            self.cmp_unit.v.eq(self.csr_reg_file.flags_data[3])
        ]

        # Shared combinational nets
        self.pc_next: Signal = Signal(16)
        self.pc_carry: Signal = Signal(1)
        self.imm_sext: Signal = Signal(16)
        self.base: Signal = Signal(16)
        self.stack_variant: Signal = Signal(1)
        self.eff_addr: Signal = Signal(16)
        self.eff_carry: Signal = Signal(1)
        self.addr_fault: Signal = Signal(1)
        self.store_addr: Signal = Signal(16)
        self.privilege_enabled: Signal = Signal(1)
        self.user_mode: Signal = Signal(1)
        self.access_kind: Signal = Signal(3)
        self.vector_read: Signal = Signal(1)
        self.access_denied: Signal = Signal(1)
        self.access_cause: Signal = Signal(16)

        pc_sum: Signal = Signal(17)
        eff_sum: Signal = Signal(17)
        m.d.comb += [
            pc_sum.eq(self.pc + 2),
            self.pc_next.eq(pc_sum[0:16]),
            self.pc_carry.eq(pc_sum[16]),

            self.imm_sext.eq(self.decoder_unit.imm4.as_signed()),
            self.base.eq(Cat(self.gp_reg_file.read_data_b, self.gp_reg_file.read_data_a)),
            self.stack_variant.eq(self.decoder_unit.n2 == GpReg.RSCRATCH1),

            eff_sum.eq(
                Mux(self.stack_variant, self.csr_reg_file.read_data, self.base) + self.imm_sext
            ),
            self.eff_addr.eq(eff_sum[0:16]),
            self.eff_carry.eq(eff_sum[16]),
            self.addr_fault.eq(self.eff_carry ^ self.imm_sext[15]),

            self.store_addr.eq(
                Mux(self.stack_variant, self.csr_reg_file.read_data, self.eff_addr)
            ),

            self.privilege_enabled.eq((self.csr_reg_file.extns_data & Extn.PRIVILEGE).any()),
            self.user_mode.eq(self.privilege_enabled & self.csr_reg_file.privs_data)
        ]

        self.elaborate_interrupt_sampling(m)
        self.elaborate_access_check(m)

        m.d.comb += [
            self.cause_data.eq(self.csr_reg_file.cause_data),
            self.fault_data.eq(self.csr_reg_file.fault_data),
            self.pc_data.eq(self.pc),
            self.boundary_data.eq(self.boundary_passed),
            self.in_syscall_data.eq(self.in_syscall),
            self.in_interrupt_data.eq(self.in_interrupt),
            self.int_pending_data.eq(self.int_pending),
            self.user_mode_data.eq(self.user_mode)
        ]


    def elaborate_interrupt_sampling(self, m: Module) -> None:
        self.boundary_passed: Signal = Signal(1)
        self.can_interrupt: Signal = Signal(1)

        int_sync: Signal = Signal(3, init = 0b111)
        int_edge: Value = int_sync[1] & ~int_sync[2]

        m.d.sync += int_sync.eq(Cat(self.interrupt, int_sync[0:2]))
        m.d.comb += self.can_interrupt.eq(
            (self.csr_reg_file.extns_data & Extn.INTERRUPT).any() & ~self.in_interrupt
        )

        with m.If(int_edge & self.can_interrupt):
            m.d.sync += self.int_pending.eq(1)
        with m.Elif(self.boundary_passed):
            m.d.sync += self.int_pending.eq(0)


    def vector_addr(self, byte_sel: int) -> Value:
        return Cat(C(byte_sel, 1), self.vector_sel, C(0b1111111111111, 13))


    def cause_value(self, fault_kind: FaultKind, status: Value) -> Value:
        return Cat(fault_kind, C(0b00, 2), status)


    def elaborate_access_check(self, m: Module) -> None:
        mpu_denied: Value = (
            self.user_mode & ~self.vector_read
            & ~(self.csr_reg_file.mpu_caps & self.access_kind).any()
        )
        vector_write: Value = \
            (self.access_kind == AccessKind.WRITE) & (self.mem_addr[4:16] == 0xFFF)

        m.d.comb += self.csr_reg_file.mpu_query_addr.eq(self.mem_addr)
        with m.If(self.access_kind.any()):
            # Privileged access
            with m.If(mpu_denied):
                m.d.comb += [
                    self.access_denied.eq(1),
                    self.access_cause.eq(
                        self.cause_value(FaultKind.PRIV_ACCESS, self.access_kind)
                    )
                ]
            # Unmapped address
            with m.Elif(~self.mem_mapped):
                m.d.comb += [
                    self.access_denied.eq(1),
                    self.access_cause.eq(
                        self.cause_value(FaultKind.UNMAPPED_ADDRESS, self.access_kind)
                    )
                ]
            # Vector write
            with m.Elif(vector_write):
                m.d.comb += [
                    self.access_denied.eq(1),
                    self.access_cause.eq(self.cause_value(
                        FaultKind.VECTOR_WRITE,
                        Cat(C(AccessKind.WRITE, 3), C(0, 1), self.mem_addr[0:4])
                    ))
                ]
            # Invalid access
            with m.Elif(~(self.mem_caps & self.access_kind).any()):
                m.d.comb += [
                    self.access_denied.eq(1),
                    self.access_cause.eq(
                        self.cause_value(FaultKind.INVALID_ACCESS, self.access_kind)
                    )
                ]


    def advance(self, m: Module) -> None:
        with m.If(self.pc_carry):
            self.trap(m, FaultKind.PC_OVERFLOW, C(0x00, 8))
        with m.Else():
            m.d.sync += self.pc.eq(self.pc_next)
            m.next = CoreState.FETCH_LO


    def trap(self, m: Module, fault_kind: FaultKind, status: Value) -> None:
        self.trap_value(m, self.cause_value(fault_kind, status), self.pc)


    def trap_value(self, m: Module, cause: Value, loc: Value) -> None:
        m.d.sync += [
            self.cause.eq(cause),
            self.fault_loc.eq(loc),
            self.vector_sel.eq(VectorSelect.FAULT)
        ]
        m.next = CoreState.TRAP_CAUSE


    def vector_fault(self, m: Module) -> None:
        m.d.sync += [
            self.cause.eq(self.access_cause),
            self.fault_loc.eq(self.vector_addr(0))
        ]
        with m.If(self.vector_sel == VectorSelect.FAULT):
            m.next = CoreState.TRAP_HALT_CAUSE
        with m.Else():
            m.d.sync += self.vector_sel.eq(VectorSelect.FAULT)
            m.next = CoreState.TRAP_CAUSE


    def elaborate_vector_lo(self, m: Module) -> None:
        m.d.comb += [
            self.mem_addr.eq(self.vector_addr(0)),
            self.access_kind.eq(AccessKind.READ),
            self.vector_read.eq(1)
        ]
        with m.If((self.vector_sel == VectorSelect.FAULT) & self.privilege_enabled):
            m.d.comb += [
                self.csr_reg_file.write_csr_reg.eq(CsrReg.PRIVS),
                self.csr_reg_file.write_data.eq(0x0000),
                self.csr_reg_file.write_enable.eq(1)
            ]
        with m.If(self.access_denied):
            self.vector_fault(m)
        with m.Else():
            m.d.comb += self.mem_request.eq(1)
            with m.If(self.mem_ready):
                m.d.sync += self.inst[0:8].eq(self.mem_read_data)
                m.next = CoreState.VECTOR_HI


    def elaborate_vector_hi(self, m: Module) -> None:
        m.d.comb += [
            self.mem_addr.eq(self.vector_addr(1)),
            self.access_kind.eq(AccessKind.READ),
            self.vector_read.eq(1)
        ]
        with m.If(self.access_denied):
            self.vector_fault(m)
        with m.Else():
            m.d.comb += self.mem_request.eq(1)
            with m.If(self.mem_ready):
                m.d.sync += self.inst[8:16].eq(self.mem_read_data)
                m.next = CoreState.TRAP_LINK


    def elaborate_fetch_lo(self, m: Module) -> None:
        m.d.comb += [
            self.mem_addr.eq(self.pc),
            self.access_kind.eq(AccessKind.EXECUTE)
        ]
        with m.If(self.int_pending & self.can_interrupt):
            m.d.comb += self.boundary_passed.eq(1)
            m.d.sync += self.vector_sel.eq(VectorSelect.INTERRUPT)
            m.next = CoreState.VECTOR_LO
        with m.Elif(self.pc == 0xFFFF):
            m.d.comb += self.boundary_passed.eq(1)
            self.trap(m, FaultKind.PC_OVERFLOW, C(0x00, 8))
        with m.Elif(self.access_denied):
            m.d.comb += self.boundary_passed.eq(1)
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += self.mem_request.eq(1)
            with m.If(self.mem_ready):
                m.d.comb += self.boundary_passed.eq(1)
                m.d.sync += self.inst[0:8].eq(self.mem_read_data)
                m.next = CoreState.FETCH_HI


    def elaborate_fetch_hi(self, m: Module) -> None:
        m.d.comb += [
            self.mem_addr.eq(self.pc + 1),
            self.access_kind.eq(AccessKind.EXECUTE)
        ]
        with m.If(self.access_denied):
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += self.mem_request.eq(1)
            with m.If(self.mem_ready):
                m.d.sync += self.inst[8:16].eq(self.mem_read_data)
                m.next = CoreState.DECODE


    def elaborate_decode(self, m: Module) -> None:
        decoder: DecoderUnit = self.decoder_unit

        is_priv_fault: Value = (
            ((decoder.op == Op.HALT) & ~decoder.is_syscall) |
            ((decoder.op == Op.RSR) & ~decoder.csr_user_readable) |
            ((decoder.op == Op.WSR) & ~decoder.csr_user_writable) |
            (decoder.is_jmp_csr & ~decoder.csr_user_readable)
        )
        is_context_return: Value = \
            decoder.is_jmp_csr & ((decoder.csr == CsrReg.RETSC) | (decoder.csr == CsrReg.RETIR))
        is_bad_context: Value = decoder.is_jmp_csr & (
            ((decoder.csr == CsrReg.RETSC) & ~(self.in_syscall & ~self.in_interrupt)) |
            ((decoder.csr == CsrReg.RETIR) & ~self.in_interrupt)
        )

        m.d.comb += self.gp_reg_file.read_gp_reg_a.eq(decoder.n1)

        with m.If(decoder.is_illegal):
            self.trap(m, FaultKind.ILLEGAL, C(0x00, 8))
        with m.Elif(self.user_mode & is_priv_fault):
            self.trap(m, FaultKind.PRIV_INSTRUCTION, C(0x00, 8))
        with m.Elif(decoder.is_syscall & (self.in_syscall | self.in_interrupt)):
            self.trap(
                m, FaultKind.IMPROPER_SYSCALL,
                Cat(self.in_syscall, self.in_interrupt, C(0, 6))
            )
        with m.Elif(is_context_return & (decoder.cmp != CmpFlag.ALWAYS)):
            self.trap(
                m, FaultKind.COND_RETURN,
                Cat(decoder.cmp, C(0, 1), decoder.csr)
            )
        with m.Elif(is_bad_context):
            self.trap(
                m, FaultKind.IMPROPER_RETURN,
                Cat(self.in_syscall, self.in_interrupt, C(0, 2), decoder.csr)
            )
        with m.Else():
            with m.Switch(decoder.op):
                with m.Case(Op.ADD, Op.ADC, Op.SUB, Op.SBB, Op.AND, Op.OR, Op.XOR, Op.RRC):
                    m.next = CoreState.EXECUTE_ALU
                with m.Case(Op.SET):
                    m.next = CoreState.EXECUTE_SET
                with m.Case(Op.LD):
                    with m.If(self.stack_variant):
                        m.next = CoreState.EXECUTE_LD_STK
                    with m.Else():
                        m.next = CoreState.EXECUTE_LD
                with m.Case(Op.ST):
                    with m.If(self.stack_variant):
                        m.next = CoreState.EXECUTE_ST_STK
                    with m.Else():
                        m.next = CoreState.EXECUTE_ST_DATA
                with m.Case(Op.RSR):
                    m.next = CoreState.EXECUTE_RSR_HI
                with m.Case(Op.WSR):
                    m.next = CoreState.EXECUTE_WSR
                with m.Case(Op.JAL, Op.JMP):
                    with m.If(decoder.is_jmp_csr):
                        m.next = CoreState.EXECUTE_JMP_CSR
                    with m.Else():
                        m.next = CoreState.EXECUTE_BRANCH
                with m.Case(Op.HALT):
                    with m.If(decoder.is_syscall & self.pc_carry):
                        self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.LINK, 8))
                    with m.Elif(decoder.is_syscall):
                        m.d.sync += [
                            self.cause.eq(self.cause_value(
                                FaultKind.HALT, self.gp_reg_file.read_data_a
                            )),
                            self.vector_sel.eq(VectorSelect.SYSCALL)
                        ]
                        m.next = CoreState.TRAP_CAUSE
                    with m.Else():
                        m.next = CoreState.EXECUTE_HALT


    def elaborate_execute_alu(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n2),
            self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n3),

            self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
            self.gp_reg_file.write_data.eq(self.alu_unit.y),
            self.gp_reg_file.write_enable.eq(1),

            self.csr_reg_file.write_csr_reg.eq(CsrReg.FLAGS),
            self.csr_reg_file.write_data.eq(
                Cat(self.alu_unit.z, self.alu_unit.c, self.alu_unit.n, self.alu_unit.v)
            ),
            self.csr_reg_file.write_enable.eq(1)
        ]
        self.advance(m)


    def elaborate_execute_set(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
            self.gp_reg_file.write_data.eq(self.decoder_unit.imm8),
            self.gp_reg_file.write_enable.eq(1)
        ]
        self.advance(m)


    def elaborate_execute_ld(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n2),
            self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2 + 1),

            self.mem_addr.eq(self.eff_addr),
            self.access_kind.eq(AccessKind.READ)
        ]
        with m.If(self.addr_fault):
            self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.READ, 8))
        with m.Elif(self.access_denied):
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += self.mem_request.eq(1)
            with m.If(self.mem_ready):
                m.d.comb += [
                    self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
                    self.gp_reg_file.write_data.eq(self.mem_read_data),
                    self.gp_reg_file.write_enable.eq(1)
                ]
                self.advance(m)


    def elaborate_execute_ld_stk(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.read_csr_reg.eq(CsrReg.SADDR),

            self.mem_addr.eq(self.eff_addr),
            self.access_kind.eq(AccessKind.READ)
        ]
        with m.If(self.addr_fault):
            self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.SADDR, 8))
        with m.Elif(self.access_denied):
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += [
                self.mem_request.eq(1),

                self.csr_reg_file.write_csr_reg.eq(CsrReg.SADDR),
                self.csr_reg_file.write_data.eq(self.eff_addr),
                self.csr_reg_file.write_enable.eq(self.mem_ready)
            ]
            with m.If(self.mem_ready):
                m.d.comb += [
                    self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
                    self.gp_reg_file.write_data.eq(self.mem_read_data),
                    self.gp_reg_file.write_enable.eq(1)
                ]
                self.advance(m)


    def elaborate_execute_st_data(self, m: Module) -> None:
        m.d.comb += self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1)
        m.d.sync += self.data_buf.eq(self.gp_reg_file.read_data_a)
        m.next = CoreState.EXECUTE_ST


    def elaborate_execute_st(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n2),
            self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2 + 1),

            self.mem_addr.eq(self.store_addr),
            self.access_kind.eq(AccessKind.WRITE)
        ]
        with m.If(self.addr_fault):
            self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.WRITE, 8))
        with m.Elif(self.access_denied):
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += [
                self.mem_write_data.eq(self.data_buf),
                self.mem_write_enable.eq(1),
                self.mem_request.eq(1)
            ]
            with m.If(self.mem_ready):
                self.advance(m)


    def elaborate_execute_st_stk(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.read_csr_reg.eq(CsrReg.SADDR),
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),

            self.mem_addr.eq(self.store_addr),
            self.access_kind.eq(AccessKind.WRITE)
        ]
        with m.If(self.addr_fault):
            self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.SADDR, 8))
        with m.Elif(self.access_denied):
            self.trap_value(m, self.access_cause, self.pc)
        with m.Else():
            m.d.comb += [
                self.mem_write_data.eq(self.gp_reg_file.read_data_a),
                self.mem_write_enable.eq(1),
                self.mem_request.eq(1),

                self.csr_reg_file.write_csr_reg.eq(CsrReg.SADDR),
                self.csr_reg_file.write_data.eq(self.eff_addr),
                self.csr_reg_file.write_enable.eq(self.mem_ready)
            ]
            with m.If(self.mem_ready):
                self.advance(m)


    def elaborate_execute_rsr_hi(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.read_csr_reg.eq(self.decoder_unit.csr),

            self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
            self.gp_reg_file.write_data.eq(self.csr_reg_file.read_data[8:16]),
            self.gp_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.EXECUTE_RSR_LO


    def elaborate_execute_rsr_lo(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.read_csr_reg.eq(self.decoder_unit.csr),

            self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n2),
            self.gp_reg_file.write_data.eq(self.csr_reg_file.read_data[0:8]),
            self.gp_reg_file.write_enable.eq(1)
        ]
        self.advance(m)


    def elaborate_execute_wsr(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),
            self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2),

            self.csr_reg_file.write_csr_reg.eq(self.decoder_unit.csr),
            self.csr_reg_file.write_data.eq(self.base),
            self.csr_reg_file.write_enable.eq(1)
        ]
        self.advance(m)


    def elaborate_execute_branch(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),
            self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2)
        ]
        with m.If(self.cmp_unit.true):
            with m.If((self.decoder_unit.op == Op.JAL) & self.pc_carry):
                self.trap(m, FaultKind.ADDR_OVERFLOW, C(OverflowKind.LINK, 8))
            with m.Else():
                with m.If(self.decoder_unit.op == Op.JAL):
                    m.d.comb += [
                        self.csr_reg_file.write_csr_reg.eq(CsrReg.RADDR),
                        self.csr_reg_file.write_data.eq(self.pc_next),
                        self.csr_reg_file.write_enable.eq(1)
                    ]
                m.d.sync += self.pc.eq(self.base)
                m.next = CoreState.FETCH_LO
        with m.Else():
            self.advance(m)


    def elaborate_execute_jmp_csr(self, m: Module) -> None:
        m.d.comb += self.csr_reg_file.read_csr_reg.eq(self.decoder_unit.csr)

        with m.If(self.decoder_unit.csr == CsrReg.RETSC):
            with m.If(self.privilege_enabled):
                m.d.comb += [
                    self.csr_reg_file.write_csr_reg.eq(CsrReg.PRIVS),
                    self.csr_reg_file.write_data.eq(self.syscall_privs),
                    self.csr_reg_file.write_enable.eq(1)
                ]
            m.d.sync += [
                self.pc.eq(self.csr_reg_file.read_data),
                self.in_syscall.eq(0)
            ]
            m.next = CoreState.FETCH_LO
        with m.Elif(self.decoder_unit.csr == CsrReg.RETIR):
            m.next = CoreState.EXECUTE_RETIR_HI
        with m.Elif(self.cmp_unit.true):
            m.d.sync += self.pc.eq(self.csr_reg_file.read_data)
            m.next = CoreState.FETCH_LO
        with m.Else():
            self.advance(m)


    def elaborate_execute_retir_hi(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.write_gp_reg.eq(GpReg.RSCRATCH0),
            self.gp_reg_file.write_data.eq(self.int_rscratch[8:16]),
            self.gp_reg_file.write_enable.eq(1),

            self.csr_reg_file.write_csr_reg.eq(CsrReg.FLAGS),
            self.csr_reg_file.write_data.eq(self.int_flags),
            self.csr_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.EXECUTE_RETIR_LO


    def elaborate_execute_retir_lo(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.read_csr_reg.eq(CsrReg.RETIR),

            self.gp_reg_file.write_gp_reg.eq(GpReg.RSCRATCH1),
            self.gp_reg_file.write_data.eq(self.int_rscratch[0:8]),
            self.gp_reg_file.write_enable.eq(1)
        ]
        with m.If(self.privilege_enabled):
            m.d.comb += [
                self.csr_reg_file.write_csr_reg.eq(CsrReg.PRIVS),
                self.csr_reg_file.write_data.eq(self.int_privs),
                self.csr_reg_file.write_enable.eq(1)
            ]
        m.d.sync += [
            self.pc.eq(self.csr_reg_file.read_data),
            self.in_interrupt.eq(0)
        ]
        m.next = CoreState.FETCH_LO


    def elaborate_execute_halt(self, m: Module) -> None:
        m.d.comb += [
            self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),

            self.csr_reg_file.write_csr_reg.eq(CsrReg.CAUSE),
            self.csr_reg_file.write_data.eq(self.cause_value(
                FaultKind.HALT, self.gp_reg_file.read_data_a
            )),
            self.csr_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.HALTED


    def elaborate_halted(self, m: Module) -> None:
        m.next = CoreState.HALTED


    def elaborate_trap_cause(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.write_csr_reg.eq(CsrReg.CAUSE),
            self.csr_reg_file.write_data.eq(self.cause),
            self.csr_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.VECTOR_LO


    def elaborate_trap_link(self, m: Module) -> None:
        with m.Switch(self.vector_sel):
            with m.Case(VectorSelect.FAULT):
                m.d.comb += [
                    self.csr_reg_file.write_csr_reg.eq(CsrReg.FAULT),
                    self.csr_reg_file.write_data.eq(self.fault_loc),
                    self.csr_reg_file.write_enable.eq(1)
                ]
            with m.Case(VectorSelect.SYSCALL):
                m.d.comb += [
                    self.csr_reg_file.write_csr_reg.eq(CsrReg.RETSC),
                    self.csr_reg_file.write_data.eq(self.pc_next),
                    self.csr_reg_file.write_enable.eq(1)
                ]
            with m.Case(VectorSelect.INTERRUPT):
                m.d.comb += [
                    self.csr_reg_file.write_csr_reg.eq(CsrReg.RETIR),
                    self.csr_reg_file.write_data.eq(self.pc),
                    self.csr_reg_file.write_enable.eq(1)
                ]
        m.next = CoreState.TRAP_ENTER


    def elaborate_trap_enter(self, m: Module) -> None:
        with m.If(self.privilege_enabled):
            m.d.comb += [
                self.csr_reg_file.write_csr_reg.eq(CsrReg.PRIVS),
                self.csr_reg_file.write_data.eq(0x0000),
                self.csr_reg_file.write_enable.eq(1)
            ]
        with m.Switch(self.vector_sel):
            with m.Case(VectorSelect.SYSCALL):
                m.d.sync += [
                    self.syscall_privs.eq(self.csr_reg_file.privs_data),
                    self.in_syscall.eq(1)
                ]
            with m.Case(VectorSelect.INTERRUPT):
                m.d.comb += [
                    self.gp_reg_file.read_gp_reg_a.eq(GpReg.RSCRATCH0),
                    self.gp_reg_file.read_gp_reg_b.eq(GpReg.RSCRATCH1)
                ]
                m.d.sync += [
                    self.int_privs.eq(self.csr_reg_file.privs_data),
                    self.int_flags.eq(self.csr_reg_file.flags_data),
                    self.int_rscratch.eq(self.base),
                    self.in_interrupt.eq(1)
                ]
        m.d.sync += self.pc.eq(self.inst)
        m.next = CoreState.FETCH_LO


    def elaborate_trap_halt_cause(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.write_csr_reg.eq(CsrReg.CAUSE),
            self.csr_reg_file.write_data.eq(self.cause),
            self.csr_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.TRAP_HALT_FAULT


    def elaborate_trap_halt_fault(self, m: Module) -> None:
        m.d.comb += [
            self.csr_reg_file.write_csr_reg.eq(CsrReg.FAULT),
            self.csr_reg_file.write_data.eq(self.fault_loc),
            self.csr_reg_file.write_enable.eq(1)
        ]
        m.next = CoreState.HALTED

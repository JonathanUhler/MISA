"""The MISA control sequencer and datapath: an unpipelined, multi-cycle core.

Memory is reached through the single-byte port contract (see memory.py); the shared MEM_REQ/MEM_WAIT
states are the one memory engine, and a `ret` register names the continuation after each access.
`advance()` is the one sequential PC bump, which raises a PC-overflow fault on wrap, and `do_fault`
is the one fault entry (set CAUSE, clear the ring bit, vector to the fault handler).

Implemented: reset, fetch, the core ISA (SET/HALT/ALU/LD/ST/RSR/WSR/JAL/JMP/JMP-CSR), faults, and all
four extensions (Dynamic Hardware, System Call, Privilege, Interrupt).
"""

from enum import auto

from amaranth import Module, Signal, Cat, C, Value, ValueLike
from amaranth.lib import wiring, enum
from amaranth.lib.wiring import In, Out

from .types import (
    Opcode, Csr, Cmp, FLAG_C, FaultKind,
    EXT_SYSCALL, EXT_PRIVILEGE, EXT_INTERRUPT,
)
from .config import CoreInit, StateEnum
from .regfile import RegFile
from .csr import SpecFile
from .decoder import Decoder
from .alu import Alu
from .cmp import CmpUnit


class CoreState(StateEnum):
    """Control-sequencer FSM states."""
    RESET0 = auto()
    MEM_REQ = auto()
    MEM_WAIT = auto()
    FETCH0 = auto()
    EXEC = auto()
    ALU_R1 = auto()
    ALU_R2 = auto()
    ALU_WB = auto()
    LD_R1 = auto()
    LD_R2 = auto()
    ST_R1 = auto()
    ST_R2 = auto()
    ST_R3 = auto()
    LDSTK_1 = auto()
    STSTK_1 = auto()
    RSR_1 = auto()
    RSR_2 = auto()
    WSR_1 = auto()
    WSR_2 = auto()
    JAL_R1 = auto()
    JAL_R2 = auto()
    JMP_R1 = auto()
    JMP_R2 = auto()
    JMPCSR_1 = auto()
    FAULT_1 = auto()
    FAULT_2 = auto()
    SYS_1 = auto()
    SYS_2 = auto()
    SYS_3 = auto()
    IRQ_1 = auto()
    IRQ_2 = auto()
    IRQ_3 = auto()
    RETI_1 = auto()
    RETI_2 = auto()
    HALTED = auto()


class _Ret(enum.IntEnum, shape=4):
    """Continuation after a shared memory access completes (held in a register)."""
    RESET_LO = 0
    RESET_HI = 1
    FETCH_LO = 2
    FETCH_HI = 3
    LD_DONE = 4
    ST_DONE = 5
    FAULT_LO = 6
    FAULT_HI = 7
    SYS_LO = 8
    SYS_HI = 9
    IRQ_LO = 10
    IRQ_HI = 11


class Core(wiring.Component):
    # Memory port: the core is the requester.
    mem_addr: Out(16)
    mem_wdata: Out(8)
    mem_we: Out(1)
    mem_req: Out(1)
    mem_rdata: In(8)
    mem_ready: In(1)
    # Interrupt request pin (Interrupt Extension).
    irq: In(1)
    # Debug / status.
    halted: Out(1)
    halt_code: Out(8)
    dbg_fetch: Out(1)  # high while in FETCH0; used by the co-simulation harness to count retires

    def __init__(self, extns_reset: int = 0x0F, init: CoreInit = None) -> None:
        # When init is None the core starts from canonical reset (reads the reset vector); otherwise
        # it starts from the arbitrary state described by the CoreInit.
        self._extns_reset: int = extns_reset
        self._init: CoreInit = init
        super().__init__()

    def elaborate(self, platform: object) -> Module:
        m: Module = Module()
        have_init: bool = self._init is not None
        cfg: CoreInit = self._init if have_init else CoreInit()

        regfile: RegFile = RegFile(init=cfg.registers)
        spec: SpecFile = SpecFile(extns_reset=self._extns_reset,
                                  init=cfg.special if have_init else None)
        dec: Decoder = Decoder()
        alu: Alu = Alu()
        cmpu: CmpUnit = CmpUnit()
        m.submodules.regfile = regfile
        m.submodules.spec = spec
        m.submodules.dec = dec
        m.submodules.alu = alu
        m.submodules.cmpu = cmpu
        self._regfile: RegFile = regfile
        self._spec: SpecFile = spec

        pc: Signal = Signal(16, init=cfg.pc)
        pc_lo: Signal = Signal(8)
        inst_lo: Signal = Signal(8)
        inst_hi: Signal = Signal(8)
        op_addr: Signal = Signal(16)
        op_we: Signal = Signal()
        op_wdata: Signal = Signal(8)
        a_reg: Signal = Signal(8)
        b_reg: Signal = Signal(8)
        base_hi: Signal = Signal(8)
        ea: Signal = Signal(16)
        rsr_lo: Signal = Signal(8)
        cause_val: Signal = Signal(16)
        in_syscall: Signal = Signal(init=cfg.in_syscall)
        in_interrupt: Signal = Signal(init=cfg.in_interrupt)
        syscall_privs: Signal = Signal(init=cfg.syscall_privs & 1)
        int_rscratch: Signal = Signal(16, init=cfg.int_rscratch)
        int_rs_hi: Signal = Signal(8)
        int_flags: Signal = Signal(4, init=cfg.int_flags & 0xF)
        int_privs: Signal = Signal(init=cfg.int_privs & 1)
        ret: Signal = Signal(_Ret)
        # Readback references for the co-simulation harness.
        self._pc: Signal = pc
        self._in_syscall: Signal = in_syscall
        self._in_interrupt: Signal = in_interrupt
        self._syscall_privs: Signal = syscall_privs
        self._int_rscratch: Signal = int_rscratch
        self._int_flags: Signal = int_flags
        self._int_privs: Signal = int_privs

        # Interrupt pin: two-FF synchronizer and rising-edge-latched pending bit.
        irq_sync0: Signal = Signal()
        irq_sync1: Signal = Signal()
        irq_prev: Signal = Signal()
        irq_pending: Signal = Signal()
        m.d.sync += [irq_sync0.eq(self.irq), irq_sync1.eq(irq_sync0), irq_prev.eq(irq_sync1)]
        with m.If(irq_sync1 & ~irq_prev):
            m.d.sync += irq_pending.eq(1)

        m.d.comb += dec.inst.eq(Cat(inst_lo, inst_hi))
        imm_sext: Value = Cat(dec.out.imm4, dec.out.imm4[3].replicate(12))  # 4-bit 2's comp -> 16
        low_idx: Value = (dec.out.n2 + 1)[:4]  # low register of a base pair, wrapping mod 16

        m.d.comb += [
            self.mem_addr.eq(op_addr),
            self.mem_we.eq(op_we),
            self.mem_wdata.eq(op_wdata),
            self.mem_req.eq(0),
            regfile.raddr.eq(0),
            regfile.waddr.eq(0),
            regfile.wdata.eq(0),
            regfile.we.eq(0),
            spec.raddr.eq(Csr.SADDR),
            spec.waddr.eq(Csr.SADDR),
            spec.wdata.eq(0),
            spec.we.eq(0),
            spec.flags_we.eq(0),
            spec.flags_in.eq(0),
            alu.op.eq(dec.out.opcode),
            alu.a.eq(a_reg),
            alu.b.eq(b_reg),
            alu.carry_in.eq(spec.flags_out[FLAG_C]),
            cmpu.flags.eq(spec.flags_out),
            cmpu.sel.eq(dec.out.cmp),
            self.dbg_fetch.eq(0),
        ]

        def issue(addr: ValueLike, we: ValueLike = 0, wdata: ValueLike = 0,
                  then: _Ret = None) -> None:
            """Set up a memory transaction and jump to the shared MEM_REQ engine."""
            m.d.sync += [op_addr.eq(addr), op_we.eq(we), op_wdata.eq(wdata)]
            if then is not None:
                m.d.sync += ret.eq(then)
            m.next = CoreState.MEM_REQ

        def do_fault(kind: int, status: ValueLike = None) -> None:
            """Enter the fault handler: latch CAUSE (reason/type + status) and vector via FAULT_1."""
            status: ValueLike = C(0, 8) if status is None else status
            m.d.sync += cause_val.eq(Cat(C(kind, 6), C(0, 2), status))
            m.next = CoreState.FAULT_1

        def advance() -> None:
            """Sequential PC += 2, raising a PC-overflow fault when it wraps past 0xFFFF."""
            with m.If((pc + 2)[16]):
                do_fault(int(FaultKind.PC_OVERFLOW))
            with m.Else():
                m.d.sync += pc.eq(pc + 2)
                m.next = CoreState.FETCH0

        fsm_init: CoreState
        if not have_init:
            fsm_init = CoreState.RESET0       # canonical reset: read the reset vector
        elif cfg.halted:
            fsm_init = CoreState.HALTED        # a halted state stays halted
        else:
            fsm_init = CoreState.FETCH0        # arbitrary state: begin fetching at the given pc

        with m.FSM(init=fsm_init):
            with m.State(CoreState.RESET0):
                issue(0xFFFE, then=_Ret.RESET_LO)

            with m.State(CoreState.MEM_REQ):
                m.d.comb += self.mem_req.eq(1)
                m.next = CoreState.MEM_WAIT

            with m.State(CoreState.MEM_WAIT):
                with m.If(self.mem_ready):
                    with m.Switch(ret):
                        with m.Case(_Ret.RESET_LO):
                            m.d.sync += pc_lo.eq(self.mem_rdata)
                            issue(0xFFFF, then=_Ret.RESET_HI)
                        with m.Case(_Ret.RESET_HI):
                            m.d.sync += pc.eq(Cat(pc_lo, self.mem_rdata))
                            m.next = CoreState.FETCH0
                        with m.Case(_Ret.FETCH_LO):
                            m.d.sync += inst_lo.eq(self.mem_rdata)
                            issue(pc + 1, then=_Ret.FETCH_HI)
                        with m.Case(_Ret.FETCH_HI):
                            m.d.sync += inst_hi.eq(self.mem_rdata)
                            m.next = CoreState.EXEC
                        with m.Case(_Ret.LD_DONE):
                            m.d.comb += [
                                regfile.waddr.eq(dec.out.n1),
                                regfile.wdata.eq(self.mem_rdata),
                                regfile.we.eq(1),
                            ]
                            advance()
                        with m.Case(_Ret.ST_DONE):
                            advance()
                        with m.Case(_Ret.FAULT_LO):
                            m.d.sync += pc_lo.eq(self.mem_rdata)
                            issue(0xFFFD, then=_Ret.FAULT_HI)
                        with m.Case(_Ret.FAULT_HI):
                            m.d.sync += pc.eq(Cat(pc_lo, self.mem_rdata))
                            m.next = CoreState.FETCH0
                        with m.Case(_Ret.SYS_LO):
                            m.d.sync += pc_lo.eq(self.mem_rdata)
                            issue(0xFFFB, then=_Ret.SYS_HI)
                        with m.Case(_Ret.SYS_HI):
                            m.d.sync += pc.eq(Cat(pc_lo, self.mem_rdata))
                            m.next = CoreState.FETCH0
                        with m.Case(_Ret.IRQ_LO):
                            m.d.sync += pc_lo.eq(self.mem_rdata)
                            issue(0xFFF9, then=_Ret.IRQ_HI)
                        with m.Case(_Ret.IRQ_HI):
                            m.d.sync += pc.eq(Cat(pc_lo, self.mem_rdata))
                            m.next = CoreState.FETCH0

            with m.State(CoreState.FETCH0):
                m.d.comb += self.dbg_fetch.eq(1)
                # Interrupts are taken here, between instructions, when enabled and not already in an
                # interrupt context.
                can_int: Value = spec.extns_out[EXT_INTERRUPT] & ~in_interrupt & irq_pending
                with m.If(can_int):
                    m.next = CoreState.IRQ_1
                with m.Else():
                    issue(pc, then=_Ret.FETCH_LO)

            with m.State(CoreState.EXEC):
                sysc: Value = spec.extns_out[EXT_SYSCALL]
                intr: Value = spec.extns_out[EXT_INTERRUPT]
                priv: Value = spec.extns_out[EXT_PRIVILEGE]

                def unavail(csr_field: Value) -> Value:
                    return (((csr_field == Csr.RETSC) & ~sysc)
                            | ((csr_field == Csr.RETIR) & ~intr)
                            | ((csr_field == Csr.PRIVS) & ~priv))

                is_rsr_wsr: Value = (dec.out.opcode == Opcode.RSR) | (dec.out.opcode == Opcode.WSR)
                is_jmpcsr: Value = (dec.out.opcode == Opcode.JMP) & dec.out.csr_target
                csr_bad: Value = (is_rsr_wsr & unavail(dec.out.n3)) | (is_jmpcsr & unavail(dec.out.n1))

                user_mode: Value = priv & spec.privs_out

                def user_readable(csr_field: Value) -> Value:
                    return ((csr_field == Csr.SADDR) | (csr_field == Csr.RADDR)
                            | (csr_field == Csr.FLAGS) | (csr_field == Csr.PRIVS))

                def user_writable(csr_field: Value) -> Value:
                    return ((csr_field == Csr.SADDR) | (csr_field == Csr.RADDR)
                            | (csr_field == Csr.FLAGS))

                with m.If(dec.out.illegal | csr_bad):
                    do_fault(int(FaultKind.ILLEGAL))
                with m.Else():
                    with m.Switch(dec.out.opcode):
                        with m.Case(Opcode.SET):
                            m.d.comb += [
                                regfile.waddr.eq(dec.out.n1),
                                regfile.wdata.eq(dec.out.imm8),
                                regfile.we.eq(1),
                            ]
                            advance()
                        with m.Case(Opcode.HALT):
                            with m.If(dec.out.syscall_bit & sysc):
                                with m.If(in_syscall | in_interrupt):
                                    do_fault(int(FaultKind.IMPROPER_SYSCALL),
                                             status=Cat(in_syscall, in_interrupt, C(0, 6)))
                                with m.Else():
                                    m.next = CoreState.SYS_1
                            with m.Elif(user_mode):
                                do_fault(int(FaultKind.PRIV_INSTRUCTION))
                            with m.Else():
                                m.d.comb += [
                                    regfile.raddr.eq(dec.out.n1),
                                    spec.waddr.eq(Csr.CAUSE),
                                    spec.wdata.eq(Cat(C(0x01, 8), regfile.rdata)),
                                    spec.we.eq(1),
                                ]
                                m.d.sync += [self.halted.eq(1), self.halt_code.eq(regfile.rdata)]
                                m.next = CoreState.HALTED
                        with m.Case(Opcode.ADD, Opcode.ADC, Opcode.SUB, Opcode.SBB,
                                    Opcode.AND, Opcode.OR, Opcode.XOR, Opcode.RRC):
                            m.next = CoreState.ALU_R1
                        with m.Case(Opcode.LD):
                            with m.If(dec.out.n2 == 0xF):
                                m.next = CoreState.LDSTK_1
                            with m.Else():
                                m.next = CoreState.LD_R1
                        with m.Case(Opcode.ST):
                            with m.If(dec.out.n2 == 0xF):
                                m.next = CoreState.STSTK_1
                            with m.Else():
                                m.next = CoreState.ST_R1
                        with m.Case(Opcode.RSR):
                            with m.If(user_mode & ~user_readable(dec.out.n3)):
                                do_fault(int(FaultKind.PRIV_INSTRUCTION))
                            with m.Else():
                                m.next = CoreState.RSR_1
                        with m.Case(Opcode.WSR):
                            with m.If(user_mode & ~user_writable(dec.out.n3)):
                                do_fault(int(FaultKind.PRIV_INSTRUCTION))
                            with m.Else():
                                m.next = CoreState.WSR_1
                        with m.Case(Opcode.JAL):
                            m.next = CoreState.JAL_R1
                        with m.Case(Opcode.JMP):
                            with m.If(dec.out.csr_target):
                                m.next = CoreState.JMPCSR_1
                            with m.Else():
                                m.next = CoreState.JMP_R1
                        with m.Default():
                            advance()

            with m.State(CoreState.ALU_R1):
                m.d.comb += regfile.raddr.eq(dec.out.n2)
                m.d.sync += a_reg.eq(regfile.rdata)
                m.next = CoreState.ALU_R2

            with m.State(CoreState.ALU_R2):
                m.d.comb += regfile.raddr.eq(dec.out.n3)
                m.d.sync += b_reg.eq(regfile.rdata)
                m.next = CoreState.ALU_WB

            with m.State(CoreState.ALU_WB):
                m.d.comb += [
                    regfile.waddr.eq(dec.out.n1),
                    regfile.wdata.eq(alu.result),
                    regfile.we.eq(1),
                    spec.flags_we.eq(1),
                    spec.flags_in.eq(Cat(alu.z, alu.c, alu.n, alu.v)),
                ]
                advance()

            with m.State(CoreState.LD_R1):
                m.d.comb += regfile.raddr.eq(dec.out.n2)
                m.d.sync += base_hi.eq(regfile.rdata)
                m.next = CoreState.LD_R2

            with m.State(CoreState.LD_R2):
                m.d.comb += regfile.raddr.eq(low_idx)
                issue(Cat(regfile.rdata, base_hi) + imm_sext, then=_Ret.LD_DONE)

            with m.State(CoreState.ST_R1):
                m.d.comb += regfile.raddr.eq(dec.out.n2)
                m.d.sync += base_hi.eq(regfile.rdata)
                m.next = CoreState.ST_R2

            with m.State(CoreState.ST_R2):
                m.d.comb += regfile.raddr.eq(low_idx)
                m.d.sync += ea.eq(Cat(regfile.rdata, base_hi) + imm_sext)
                m.next = CoreState.ST_R3

            with m.State(CoreState.ST_R3):
                m.d.comb += regfile.raddr.eq(dec.out.n1)
                issue(ea, we=1, wdata=regfile.rdata, then=_Ret.ST_DONE)

            with m.State(CoreState.LDSTK_1):
                # Stack load: SADDR += imm, then read mem[SADDR'].
                m.d.comb += spec.raddr.eq(Csr.SADDR)
                new_saddr: Value = spec.rdata + imm_sext
                m.d.comb += [spec.waddr.eq(Csr.SADDR), spec.wdata.eq(new_saddr), spec.we.eq(1)]
                issue(new_saddr, then=_Ret.LD_DONE)

            with m.State(CoreState.STSTK_1):
                # Stack store: write mem[SADDR] = reg[rd], then SADDR += imm.
                m.d.comb += [spec.raddr.eq(Csr.SADDR), regfile.raddr.eq(dec.out.n1)]
                m.d.comb += [
                    spec.waddr.eq(Csr.SADDR),
                    spec.wdata.eq(spec.rdata + imm_sext),
                    spec.we.eq(1),
                ]
                issue(spec.rdata, we=1, wdata=regfile.rdata, then=_Ret.ST_DONE)

            with m.State(CoreState.RSR_1):
                m.d.comb += [
                    spec.raddr.eq(dec.out.n3),
                    regfile.waddr.eq(dec.out.n1),
                    regfile.wdata.eq(spec.rdata[8:16]),
                    regfile.we.eq(1),
                ]
                m.d.sync += rsr_lo.eq(spec.rdata[0:8])
                m.next = CoreState.RSR_2

            with m.State(CoreState.RSR_2):
                m.d.comb += [
                    regfile.waddr.eq(dec.out.n2),
                    regfile.wdata.eq(rsr_lo),
                    regfile.we.eq(1),
                ]
                advance()

            with m.State(CoreState.WSR_1):
                m.d.comb += regfile.raddr.eq(dec.out.n1)
                m.d.sync += base_hi.eq(regfile.rdata)
                m.next = CoreState.WSR_2

            with m.State(CoreState.WSR_2):
                m.d.comb += [
                    regfile.raddr.eq(dec.out.n2),
                    spec.waddr.eq(dec.out.n3),
                    spec.wdata.eq(Cat(regfile.rdata, base_hi)),
                    spec.we.eq(1),
                ]
                advance()

            with m.State(CoreState.JAL_R1):
                m.d.comb += regfile.raddr.eq(dec.out.n1)
                m.d.sync += base_hi.eq(regfile.rdata)
                m.next = CoreState.JAL_R2

            with m.State(CoreState.JAL_R2):
                m.d.comb += regfile.raddr.eq(dec.out.n2)
                with m.If(cmpu.taken):
                    m.d.comb += [spec.waddr.eq(Csr.RADDR), spec.wdata.eq(pc + 2), spec.we.eq(1)]
                    m.d.sync += pc.eq(Cat(regfile.rdata, base_hi))
                    m.next = CoreState.FETCH0
                with m.Else():
                    advance()

            with m.State(CoreState.JMP_R1):
                m.d.comb += regfile.raddr.eq(dec.out.n1)
                m.d.sync += base_hi.eq(regfile.rdata)
                m.next = CoreState.JMP_R2

            with m.State(CoreState.JMP_R2):
                m.d.comb += regfile.raddr.eq(dec.out.n2)
                with m.If(cmpu.taken):
                    m.d.sync += pc.eq(Cat(regfile.rdata, base_hi))
                    m.next = CoreState.FETCH0
                with m.Else():
                    advance()

            with m.State(CoreState.JMPCSR_1):
                # RETSC and RETIR carry context-return behavior; an ordinary CSR just loads PC.
                m.d.comb += spec.raddr.eq(dec.out.n1)
                with m.If(cmpu.taken):
                    with m.If(dec.out.n1 == Csr.RETSC):
                        with m.If((dec.out.cmp == Cmp.ALWAYS) & in_syscall & ~in_interrupt):
                            m.d.comb += [
                                spec.waddr.eq(Csr.PRIVS),
                                spec.wdata.eq(syscall_privs),
                                spec.we.eq(1),
                            ]
                            m.d.sync += [pc.eq(spec.rdata), in_syscall.eq(0)]
                            m.next = CoreState.FETCH0
                        with m.Elif(dec.out.cmp == Cmp.ALWAYS):
                            do_fault(int(FaultKind.IMPROPER_RETURN),
                                     status=Cat(in_syscall, in_interrupt, C(0, 2),
                                                C(int(Csr.RETSC), 4)))
                        with m.Else():
                            do_fault(int(FaultKind.COND_RETURN),
                                     status=Cat(dec.out.cmp, C(0, 1), C(int(Csr.RETSC), 4)))
                    with m.Elif(dec.out.n1 == Csr.RETIR):
                        with m.If((dec.out.cmp == Cmp.ALWAYS) & in_interrupt):
                            m.next = CoreState.RETI_1
                        with m.Elif(dec.out.cmp == Cmp.ALWAYS):
                            do_fault(int(FaultKind.IMPROPER_RETURN),
                                     status=Cat(in_syscall, in_interrupt, C(0, 2),
                                                C(int(Csr.RETIR), 4)))
                        with m.Else():
                            do_fault(int(FaultKind.COND_RETURN),
                                     status=Cat(dec.out.cmp, C(0, 1), C(int(Csr.RETIR), 4)))
                    with m.Else():
                        m.d.sync += pc.eq(spec.rdata)
                        m.next = CoreState.FETCH0
                with m.Else():
                    advance()

            with m.State(CoreState.FAULT_1):
                m.d.comb += [spec.waddr.eq(Csr.CAUSE), spec.wdata.eq(cause_val), spec.we.eq(1)]
                m.next = CoreState.FAULT_2

            with m.State(CoreState.FAULT_2):
                m.d.comb += [spec.waddr.eq(Csr.PRIVS), spec.wdata.eq(0), spec.we.eq(1)]
                issue(0xFFFC, then=_Ret.FAULT_LO)

            with m.State(CoreState.SYS_1):
                m.d.comb += [
                    regfile.raddr.eq(dec.out.n1),
                    spec.waddr.eq(Csr.CAUSE),
                    spec.wdata.eq(Cat(C(0x01, 8), regfile.rdata)),
                    spec.we.eq(1),
                ]
                m.next = CoreState.SYS_2

            with m.State(CoreState.SYS_2):
                m.d.comb += [spec.waddr.eq(Csr.RETSC), spec.wdata.eq(pc + 2), spec.we.eq(1)]
                m.next = CoreState.SYS_3

            with m.State(CoreState.SYS_3):
                m.d.comb += [spec.waddr.eq(Csr.PRIVS), spec.wdata.eq(0), spec.we.eq(1)]
                m.d.sync += [syscall_privs.eq(spec.privs_out), in_syscall.eq(1)]
                issue(0xFFFA, then=_Ret.SYS_LO)

            with m.State(CoreState.IRQ_1):
                # Read RSCRATCH0 (high byte of the saved pair).
                m.d.comb += regfile.raddr.eq(0xE)
                m.d.sync += int_rs_hi.eq(regfile.rdata)
                m.next = CoreState.IRQ_2

            with m.State(CoreState.IRQ_2):
                # Read RSCRATCH1 (low byte); save RETIR and the FLAGS/ring shadows; enter context.
                m.d.comb += [
                    regfile.raddr.eq(0xF),
                    spec.waddr.eq(Csr.RETIR),
                    spec.wdata.eq(pc),
                    spec.we.eq(1),
                ]
                m.d.sync += [
                    int_rscratch.eq(Cat(regfile.rdata, int_rs_hi)),
                    int_flags.eq(spec.flags_out),
                    int_privs.eq(spec.privs_out),
                    in_interrupt.eq(1),
                    irq_pending.eq(0),
                ]
                m.next = CoreState.IRQ_3

            with m.State(CoreState.IRQ_3):
                # Clear the ring bit, then fetch the handler vector at 0xFFF8/0xFFF9.
                m.d.comb += [spec.waddr.eq(Csr.PRIVS), spec.wdata.eq(0), spec.we.eq(1)]
                issue(0xFFF8, then=_Ret.IRQ_LO)

            with m.State(CoreState.RETI_1):
                # Restore RSCRATCH0 and FLAGS.
                m.d.comb += [
                    regfile.waddr.eq(0xE),
                    regfile.wdata.eq(int_rscratch[8:16]),
                    regfile.we.eq(1),
                    spec.flags_we.eq(1),
                    spec.flags_in.eq(int_flags),
                ]
                m.next = CoreState.RETI_2

            with m.State(CoreState.RETI_2):
                # Restore RSCRATCH1 and the ring bit, reload PC from RETIR, leave the context.
                m.d.comb += [
                    regfile.waddr.eq(0xF),
                    regfile.wdata.eq(int_rscratch[0:8]),
                    regfile.we.eq(1),
                    spec.raddr.eq(Csr.RETIR),
                    spec.waddr.eq(Csr.PRIVS),
                    spec.wdata.eq(int_privs),
                    spec.we.eq(1),
                ]
                m.d.sync += [pc.eq(spec.rdata), in_interrupt.eq(0)]
                m.next = CoreState.FETCH0

            with m.State(CoreState.HALTED):
                m.next = CoreState.HALTED

        return m

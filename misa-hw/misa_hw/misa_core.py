from enum import StrEnum

from amaranth import C, Cat, Module, Mux, Signal, Value
from amaranth.lib.enum import IntEnum
from amaranth.lib.wiring import Component, In, Out

from .alu_unit import AluUnit
from .cmp_unit import CmpUnit
from .csr_reg_file import CsrRegFile
from .decoder_unit import DecoderUnit
from .gp_reg_file import GpRegFile
from .grammar import CmpFlag, CsrReg, Extn, FaultKind, GpReg, Op
from .init import CoreInit


"""
MisaCore FSM design
===================

A multi-cycle state machine with no pipelining and no instruction cache. Every instruction costs at
least two memory reads just to fetch its word, so memory latency dominates and the design optimizes
for area and clarity over CPI. Two consequences shape everything below:

- A state is spent freely whenever it deletes a flop, a mux, or a special case.
- Each resource (the memory bus, one GP write port, two GP read ports, one CSR read port, one CSR
  write port) is used by at most one state at a time, so no arbitration logic exists anywhere.


Conventions
-----------

`x = v`       combinational drive, valid only while the state is active
`x <- v`      registered assignment, visible to the following state
`Next S`      the FSM moves to state S on the next clock edge
`Register[i]` general purpose register file, `Special[c]` CSR file, `Memory[a]` the memory bus
`n1 n2 n3`    the three 4-bit operand fields of `inst`, as named by `DecoderUnit`

Register file writes are combinational enables that commit on the clock edge, so they are written
`Register[i] <- v` and take effect even when the state also redirects the FSM to a trap.


Memory bus protocol
-------------------

A state that touches memory drives `mem_addr`, `mem_write_data`, `mem_write_enable` and holds
`mem_request = 1` while self-looping, then acts on `mem_read_data` in the cycle `mem_ready` is
high. Outside such states `mem_request = 0` and `mem_write_enable = 0`.

This holds no copy of the request, so every address is recomputed from `pc`, `inst`, the register
files and `vector_sel` on every cycle of the wait. All of those are stable for the whole access,
so the bus sees a constant request, and the controller is free to accept it on any cycle. A
zero-latency memory asserting `mem_ready` in the first cycle is handled by the same code, as is a
QSPI controller taking a dozen.

The only subtlety is that a state must not commit a side effect on every cycle of its wait. Where a
memory access and a register write happen in one state, the write enable is gated by `mem_ready`.


Datapath state
--------------

pc            (16) program counter
inst          (16) instruction word, and doubles as the assembly buffer for vector fetches
cause         (16) CAUSE value staged by the state that detected a trap, consumed by TRAP_CAUSE
vector_sel     (2) trap kind, and the two variable bits of the vector address
data_buf       (8) the store data byte of a non-stack ST, which needs a third GP read
in_syscall     (1) in a system call context
in_interrupt   (1) in an interrupt context
int_pending    (1) an interrupt edge has been accepted and is waiting for an instruction boundary
int_rscratch  (16) RSCRATCH shadow, saved on interrupt entry
int_flags      (4) FLAGS shadow, saved on interrupt entry
int_privs      (1) PRIVS shadow, saved on interrupt entry
syscall_privs  (1) PRIVS shadow, saved on system call entry

Neither output of the halt interface is a register. `halted` is the combinational decode of the
HALTED state, which the FSM never leaves, so HALT needs no flop of its own and a core started from
a halted `CoreInit` reports itself halted without one. `halt_code` is the high byte of the CAUSE
CSR, which HALT writes as `{Register[RS], 0x01}` and which nothing writes afterwards; reading it
costs nothing in HALTED, where the CSR read port is otherwise idle.

Shared combinational nets, named here because several states use them:

pc_next     = pc + 2, with its carry out of bit 15. Used by the sequential advance, by JAL's RADDR
              and by SYSCALL's RETSC. Only the advance treats the carry as a fault.
imm_sext    = SignExt16(inst[12:16]), the LD/ST offset
base        = Cat(gp_data_b, gp_data_a), the 16-bit value in a GP register pair, high byte on
              port A. Also forms a branch target from n1 and n2.
eff_addr    = Mux(stack_variant, Special[SADDR], base) + imm_sext, and its carry out `eff_carry`
addr_fault  = eff_carry ^ imm_sext[15]. An effective address must not wrap, and this one XOR
              detects leaving [0x0000, 0xFFFF] in either direction (verified exhaustively over all
              65536 x 16 base/offset pairs).
privilege_enabled = EXTNS.PRIVILEGE. A disabled extension keeps the contents of the registers it
              defines, so this gates every automatic write to PRIVS as well as selecting the level.
user_mode   = privilege_enabled & PRIVS.R
vector_addr = Cat(byte_sel, vector_sel, 0b1111111111111). Every vector lives in 0xFFF8 - 0xFFFF,
              so selecting one costs no mux: VectorSelect indexes the table directly.


Reset and state injection
-------------------------

A cold boot starts the FSM in VECTOR_LO with `vector_sel` reset to VectorSelect.RESET, so the
reset vector is read by the same two states every other entry uses. The register files carry their
own reset values: every GP register 0x00, every CSR 0x0000 except EXTNS, which holds the set of
extensions the hardware provides. The core's own flops reset to `in_syscall = 0`,
`in_interrupt = 0` and `int_pending = 0`, which is what the architecture means by not being
initialized into a system call or interrupt context. `cause` and `data_buf` are microarchitectural
temporaries and need no reset value, because no state reads either before writing it.

A test harness drives a `CoreInit` into the core to start from an arbitrary architectural state
instead, which is what a lockstep comparison against misa-spec needs. `CoreInit` carries the two
register file images plus `pc` and the core flops, and it selects the FSM entry state rather than
VECTOR_LO: HALTED when `halted` is set, and otherwise FETCH_LO with `pc` preloaded, which skips the
vector fetch entirely. `int_pending` is part of `CoreInit` so that a pending interrupt can be
staged identically on both sides; misa-spec carries the same field on `State`, and without it the
oracle can be placed in a state the hardware cannot be placed in.


Trap entry
----------

Reset, faults, system calls and interrupts all end by loading the PC from a vector, so they share
one five state sequence. `vector_sel` selects both the vector and the per-kind side effects, and the
detecting state stages `cause` before handing over. Reset enters at VECTOR_LO and skips the rest.

No global write-enable gate is needed to honor "a fault leaves processor state unchanged". Faults
are detected either in DECODE, which writes nothing, or in the state that computes an address,
which simply does not assert its enables. The one fault that retains its instruction's effects, a
PC overflow on the sequential advance, gets that for free because the writeback is already
committing on the edge that redirects the FSM.

Faults MUST NOT touch `in_syscall` or `in_interrupt`: the handler runs in the context of the
faulting instruction, so interrupts stay masked across a fault taken inside an interrupt.


Interrupt sampling
------------------

The pin is synchronized by two flops and edge detected into `int_pending`. The latch is set only
while an interrupt could be taken, and is cleared once an instruction boundary has been passed,
which together implement "pin activity that cannot be taken is discarded rather than retained".

Both of those are conditions on one flop, so their priority is part of the design:

    If edge & EXTNS.INTERRUPT & ~in_interrupt
      int_pending <- 1
    Elif boundary_passed
      int_pending <- 0

The set wins, so an edge arriving in the same cycle the boundary is passed is kept for the next
boundary rather than dropped. `boundary_passed` is asserted only on the cycle FETCH_LO is left, not
on every cycle of its memory wait, which would otherwise discard an edge arriving mid-fetch.

The whole synchronizer chain resets to the pin's active level. A pin already active when reset is
released therefore produces no edge, which is exactly the required "discard pre-reset activity",
and a pin that is inactive at reset and later asserted is still seen.


States
------

VECTOR_LO
  Reads the low byte of the vector selected by `vector_sel` into the instruction register, which is
  dead at this point and saves a dedicated byte buffer.
    mem_addr = vector_addr(0), mem_request = 1
    If mem_ready
      inst[0:8] <- mem_read_data
      Next VECTOR_HI

VECTOR_HI
    mem_addr = vector_addr(1), mem_request = 1
    If mem_ready
      pc <- Cat(inst[0:8], mem_read_data)
      Next FETCH_LO

FETCH_LO
  The instruction boundary, and the only place an interrupt is taken. Interrupt entry outranks the
  PC overflow check, because a preempted instruction raises its fault after the interrupt returns.
    If int_pending & EXTNS.INTERRUPT & ~in_interrupt
      boundary_passed = 1
      Next TRAP_LINK with vector_sel <- VectorSelect.INTERRUPT
    Elif pc == 0xFFFF
      The high byte of the instruction word lies outside the address space.
      boundary_passed = 1
      TRAP(PC_OVERFLOW, status = 0x00)
    Else
      mem_addr = pc, mem_request = 1
      If mem_ready
        inst[0:8] <- mem_read_data
        boundary_passed = 1
        Next FETCH_HI

FETCH_HI
    mem_addr = pc + 1, mem_request = 1
    If mem_ready
      inst[8:16] <- mem_read_data
      Next DECODE

DECODE
  Pure dispatch: it writes no architectural state, raises every fault that is a property of the
  instruction word plus the current context, and selects an execute state. Keeping it separate from
  both FETCH_HI and the execute states costs one cycle per instruction and keeps the decode
  conditions out of the memory data path and out of the writeback states.

  The checks are ordered, and the order is architectural:
    If decoder.is_illegal
      Covers reserved fields that are non-zero, a reserved comparison, a CSR operand whose
      extension is disabled, and SYSCALL without the System Call Extension.
      TRAP(ILLEGAL, status = 0x00)
    Elif user_mode & (op == HALT & ~is_syscall
                      | op == RSR & ~csr_user_readable
                      | op == WSR & ~csr_user_writable
                      | is_jmp_csr & ~csr_user_readable)
      Privilege outranks both context faults below, so a User Mode JMP to RETSC or RETIR reports a
      privileged instruction rather than a bad return context.
      TRAP(PRIV_INSTRUCTION, status = 0x00)
    Elif is_syscall & (in_syscall | in_interrupt)
      TRAP(IMPROPER_SYSCALL, status = Cat(in_syscall, in_interrupt, C(0, 6)))
    Elif is_jmp_csr & (csr == RETSC | csr == RETIR) & (cmp != ALWAYS)
      A property of the encoding, so it fires whether or not the comparison would be true. This is
      why both context faults are resolved here, where FLAGS is not consulted at all.
      TRAP(COND_RETURN, status = Cat(cmp, C(0, 1), csr))
    Elif is_jmp_csr & (csr == RETSC & ~(in_syscall & ~in_interrupt)
                       | csr == RETIR & ~in_interrupt)
      TRAP(IMPROPER_RETURN, status = Cat(in_syscall, in_interrupt, C(0, 2), csr))
    Else
      Switch op
        Case ADD, ADC, SUB, SBB, AND, OR, XOR, RRC  Next EXECUTE_ALU
        Case SET                                    Next EXECUTE_SET
        Case LD                                     Next EXECUTE_LD_STK or EXECUTE_LD
        Case ST                                     Next EXECUTE_ST_STK or EXECUTE_ST_DATA
        Case RSR                                    Next EXECUTE_RSR_HI
        Case WSR                                    Next EXECUTE_WSR
        Case JAL, JMP                               Next EXECUTE_JMP_CSR or EXECUTE_BRANCH
        Case HALT
          Opcode 0000 is terminal when E is 0 and a system call when E is 1. Both encode the same
          CAUSE value, so a SYSCALL in a legal context is staged here and handed to the shared trap
          sequence. This is the one dispatch that reads a register.
          If is_syscall
            cause <- Cat(C(HALT, 6), C(0, 2), Register[n1])
            vector_sel <- VectorSelect.SYSCALL
            Next TRAP_CAUSE
          Else
            Next EXECUTE_HALT
      The LD and ST cases take the stack variant when n2 == 0xF, and the JAL/JMP case takes the
      special register target variant when is_jmp_csr.

EXECUTE_ALU
  One state: both operands come from the two read ports, and `AluOp` is a subset of `Op` so the
  opcode drives the ALU directly with no translation.
    alu.a = Register[n2], alu.b = Register[n3], alu.cin = FLAGS.C, alu.op = op
    Register[n1] <- alu.y
    Special[FLAGS] <- Cat(alu.z, alu.c, alu.n, alu.v)
    ADVANCE
  The ALU unit already folds the per-instruction flag rules in, including RRC taking C from the
  shifted out LSB of RS and the logic ops clearing C and V. Flags come from the computed value, not
  from the destination, so an RD of R0 still updates FLAGS.

EXECUTE_SET
    Register[n1] <- inst[8:16]
    ADVANCE

EXECUTE_LD
  The base pair and the offset are all functions of `inst`, so `eff_addr` is stable for the whole
  access and needs no staging register. The low half of the pair is register n2 + 1, which never
  wraps here: n2 == 0xF is the stack variant and was dispatched elsewhere, so n2 is at most 0xE.
    gp_reg_a = n2, gp_reg_b = n2 + 1
    If addr_fault
      TRAP(ADDR_OVERFLOW, status = 0x00)
    Else
      mem_addr = eff_addr, mem_request = 1
      If mem_ready
        Register[n1] <- mem_read_data
        ADVANCE

EXECUTE_LD_STK
  A pop reads from the updated stack address, so SADDR is written before the access. The write
  enable is gated by `mem_ready` so that self-looping does not apply the offset repeatedly.
    read_csr_reg = SADDR
    If addr_fault
      TRAP(ADDR_OVERFLOW, status = 0x00)
    Else
      mem_addr = eff_addr, mem_request = 1
      Special[SADDR] <- eff_addr, write_enable = mem_ready
      If mem_ready
        Register[n1] <- mem_read_data
        ADVANCE

EXECUTE_ST_DATA
  A non-stack store needs three GP reads, one more than there are ports, so the data byte is read a
  state early. This is the cheaper half of the choice: eight flops here, or sixteen to stage the
  effective address instead, or a third read port.
    data_buf <- Register[n1]
    Next EXECUTE_ST

EXECUTE_ST
    gp_reg_a = n2, gp_reg_b = n2 + 1
    If addr_fault
      TRAP(ADDR_OVERFLOW, status = 0x00)
    Else
      mem_addr = eff_addr, mem_write_data = data_buf, mem_write_enable = 1, mem_request = 1
      If mem_ready
        ADVANCE

EXECUTE_ST_STK
  A push writes to the current stack address and then applies the offset, the mirror of a pop. Only
  one GP read is needed, so no data staging. Note that the bound check is on the updated SADDR
  while the access uses the old one, and a faulting push must not store.
    read_csr_reg = SADDR
    If addr_fault
      TRAP(ADDR_OVERFLOW, status = 0x00)
    Else
      mem_addr = Special[SADDR], mem_write_data = Register[n1], mem_write_enable = 1
      mem_request = 1
      Special[SADDR] <- eff_addr, write_enable = mem_ready
      If mem_ready
        ADVANCE

EXECUTE_RSR_HI
  Two GP writes over two states, since there is one write port. The CSR is simply read again in the
  second state rather than staging its low byte, which deletes a register for free.
    Register[n1] <- Special[csr][8:16]
    Next EXECUTE_RSR_LO

EXECUTE_RSR_LO
    Register[n2] <- Special[csr][0:8]
    ADVANCE
  When n1 == n2 the low byte wins, matching the reference model's write order.

EXECUTE_WSR
    Special[csr] <- Cat(Register[n2], Register[n1])
    ADVANCE
  The CSR file owns the per-register write masks, the read-as-zero reserved bits, and the EXTNS
  rule that bit 0 ignores writes and that writes are dropped entirely without the Dynamic Hardware
  Extension. A write to FLAGS lands on the same four bits the ALU writes. Because the value commits
  on this state's edge and the next DECODE is at least three cycles later, a new EXTNS or PRIVS
  value is automatically in effect for the following instruction.

EXECUTE_BRANCH
  JAL and JMP with a register pair target, in one state: both target bytes read at once, and the
  RADDR link uses the free CSR write port.
    target = Cat(Register[n2], Register[n1])
    If cmp_unit.true
      If op == JAL
        Special[RADDR] <- pc + 2
      pc <- target
      Next FETCH_LO
    Else
      ADVANCE
  The RADDR link is not range checked, and neither is the branch target. See ADVANCE.

EXECUTE_JMP_CSR
  Every illegal or ill-contexted form was already rejected in DECODE, so only three good cases
  remain. A RETSC return fits in one state; RETIR needs two for its second GP write.
    If csr == RETSC
      If privilege_enabled
        Special[PRIVS] <- syscall_privs
      pc <- Special[RETSC], in_syscall <- 0
      Next FETCH_LO
    Elif csr == RETIR
      Next EXECUTE_RETIR_HI
    Elif cmp_unit.true
      pc <- Special[csr]
      Next FETCH_LO
    Else
      ADVANCE
  RETSC and RETIR reached here only with ALWAYS, so they ignore the comparison. Their saved ring
  bits occupy independent slots, so an interrupt taken inside a system call restores the two values
  independently. Both restores are gated: a handler which disabled the Privilege Extension must not
  put a ring bit back into a register the extension no longer governs, since the bit would become
  effective again the moment the extension was.

EXECUTE_RETIR_HI
    Register[RSCRATCH0] <- int_rscratch[8:16]
    Special[FLAGS] <- int_flags
    Next EXECUTE_RETIR_LO

EXECUTE_RETIR_LO
    Register[RSCRATCH1] <- int_rscratch[0:8]
    If privilege_enabled
      Special[PRIVS] <- int_privs
    pc <- Special[RETIR], in_interrupt <- 0
    Next FETCH_LO
  Leaving the interrupt context is what re-enables interrupts, so no separate mask exists. The two
  states are not interruptible because `int_pending` is only consulted at FETCH_LO.

EXECUTE_HALT
  Terminal, and does not advance the PC.
    Special[CAUSE] <- Cat(C(HALT, 6), C(0, 2), Register[n1])
    Next HALTED

HALTED
    halted = 1
    halt_code = Special[CAUSE][8:16]
    Next HALTED
  Unconditional: a halted processor takes no interrupts.

TRAP_CAUSE
  Entered with `cause` staged and `vector_sel` set. Interrupts do not disturb CAUSE, so they enter
  one state later at TRAP_LINK.
    Special[CAUSE] <- cause
    Next TRAP_LINK

TRAP_LINK
  The return or diagnostic address, selected by the trap kind. A fault records where it happened; a
  PC overflow on a sequential advance therefore records the last instruction before the overflow,
  and one raised by a PC of 0xFFFF records 0xFFFF.
    Switch vector_sel
      Case FAULT      Special[FAULT] <- pc
      Case SYSCALL    Special[RETSC] <- pc + 2
      Case INTERRUPT  Special[RETIR] <- pc
    Next TRAP_ENTER
  The interrupt return address is the preempted instruction itself, not the one after it.

TRAP_ENTER
  Drops to Machine Mode for every kind, and saves whatever the kind has to restore on return. The
  interrupt shadow of RSCRATCH needs both read ports, which are free here.

  The shadows read the pre-write value of PRIVS and FLAGS while the same edge writes PRIVS, which
  is the intended order: a return restores the ring bit the trap displaced. Neither read goes
  through the CSR read port, because `CsrRegFile` exposes `flags_data` and `privs_data` directly.

  Only the write is gated by `privilege_enabled`. Without the extension the processor is already in
  Machine Mode whatever PRIVS holds, so there is nothing to drop, and the register belongs to a
  disabled extension and must keep its contents. The shadows are saved either way, so a handler
  which re-enables the extension before returning restores the bit the trap actually displaced.
    If privilege_enabled
      Special[PRIVS] <- 0x0000
    Switch vector_sel
      Case FAULT
        Nothing further. A fault must not change either context flag.
      Case SYSCALL
        syscall_privs <- PRIVS.R, in_syscall <- 1
      Case INTERRUPT
        int_privs <- PRIVS.R, in_interrupt <- 1
        int_flags <- FLAGS
        int_rscratch <- Cat(Register[RSCRATCH1], Register[RSCRATCH0])
    Next VECTOR_LO


Shared actions
--------------

These are not states. They are the tails that several states end with.

ADVANCE
  The only place a sequential PC overflow is raised. 0xFFFE is its only reachable source, since
  0xFFFF is caught at fetch, and the faulting instruction's effects are retained because its
  writeback is already committing on this edge.
    If pc_next carries out of bit 15
      TRAP(PC_OVERFLOW, status = 0x00)
    Else
      pc <- pc + 2
      Next FETCH_LO

  This is the only consumer of `pc_next` that treats the carry as a fault. The same incrementer
  feeds JAL's RADDR in EXECUTE_BRANCH and SYSCALL's RETSC in TRAP_LINK as link addresses, and both
  of those wrap silently: a JAL or a SYSCALL at 0xFFFE links to 0x0000 without faulting. Branch
  targets are not checked either, so a branch to 0xFFFF is taken, and the fault is raised by the
  following fetch rather than by the branch.

TRAP(fault_kind, status)
  Only faults use this tail. System call and interrupt entry stage `cause` and `vector_sel`
  themselves, in DECODE and FETCH_LO respectively.
    cause <- Cat(C(fault_kind, 6), C(0, 2), status)
    vector_sel <- VectorSelect.FAULT
    Next TRAP_CAUSE


Deliberate omissions
--------------------

- No memory protection unit, so SADDR is the full 16 bits and the "Privileged access" fault is
  never raised. User Mode is exactly the three instruction restrictions checked in DECODE, and the
  stack bound is the same bound as any other effective address.
- Only one of the two sources of "Address overflow" exists. The architecture raises it both for an
  effective address that leaves the 16-bit range and for one that names memory the implementation
  does not provide; `addr_fault` covers the first. The second is unreachable rather than omitted,
  because the memory controller maps the whole 16-bit space, and it would have to come back if a
  variant ever left part of the range unmapped.
- No merging of DECODE into FETCH_HI, and none of the single state execute paths folded back into
  DECODE. Either would save a cycle on an instruction that already costs two memory accesses,
  at the price of putting the dispatch conditions on the memory read path or duplicating them.
- EXECUTE_LD/EXECUTE_LD_STK and EXECUTE_ST/EXECUTE_ST_STK are kept apart rather than muxed on
  `n2 == 0xF`, because the stack variants differ in the order of their SADDR update, not just in
  the address source.
"""


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

    TRAP_CAUSE = "TRAP_CAUSE"
    TRAP_LINK  = "TRAP_LINK"
    TRAP_ENTER = "TRAP_ENTER"


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


    def __init__(self, init_data: CoreInit = None):
        # A cold boot reads the reset vector; an injected state starts from the architectural state
        # it carries, which is what a lockstep comparison against misa-spec needs
        self.init_data: CoreInit = init_data or CoreInit()
        self.init_state: CoreState = CoreState.VECTOR_LO
        if init_data is not None:
            self.init_state = CoreState.HALTED if init_data.halted else CoreState.FETCH_LO
        super().__init__()


    def elaborate(self, platform: object) -> Module:
        m: Module = Module()

        self.elaborate_submodules(m)
        self.elaborate_datapath(m)

        with m.FSM(init = self.init_state):
            self.elaborate_vector_lo(m)
            self.elaborate_vector_hi(m)
            self.elaborate_fetch_lo(m)
            self.elaborate_fetch_hi(m)
            self.elaborate_decode(m)
            self.elaborate_execute_alu(m)
            self.elaborate_execute_set(m)
            self.elaborate_execute_ld(m)
            self.elaborate_execute_ld_stk(m)
            self.elaborate_execute_st_data(m)
            self.elaborate_execute_st(m)
            self.elaborate_execute_st_stk(m)
            self.elaborate_execute_rsr_hi(m)
            self.elaborate_execute_rsr_lo(m)
            self.elaborate_execute_wsr(m)
            self.elaborate_execute_branch(m)
            self.elaborate_execute_jmp_csr(m)
            self.elaborate_execute_retir_hi(m)
            self.elaborate_execute_retir_lo(m)
            self.elaborate_execute_halt(m)
            self.elaborate_halted(m)
            self.elaborate_trap_cause(m)
            self.elaborate_trap_link(m)
            self.elaborate_trap_enter(m)

        return m


    def elaborate_submodules(self, m: Module) -> None:
        self.alu_unit = AluUnit()
        self.cmp_unit = CmpUnit()
        self.csr_reg_file = CsrRegFile(self.init_data.special)
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
        self.privilege_enabled: Signal = Signal(1)
        self.user_mode: Signal = Signal(1)

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

            self.privilege_enabled.eq((self.csr_reg_file.extns_data & Extn.PRIVILEGE).any()),
            self.user_mode.eq(self.privilege_enabled & self.csr_reg_file.privs_data)
        ]

        self.elaborate_interrupt_sampling(m)


    def elaborate_interrupt_sampling(self, m: Module) -> None:
        # The interrupt pin is active-high. Two flops synchronize it and a third holds the previous
        # sample for the edge detector; the whole chain resets to the active level, so pin activity
        # which predates the release of reset produces no edge.
        self.boundary_passed: Signal = Signal(1)
        self.can_interrupt: Signal = Signal(1)

        int_sync: Signal = Signal(3, init = 0b111)
        int_edge: Value = int_sync[1] & ~int_sync[2]

        m.d.sync += int_sync.eq(Cat(self.interrupt, int_sync[0:2]))
        m.d.comb += self.can_interrupt.eq(
            (self.csr_reg_file.extns_data & Extn.INTERRUPT).any() & ~self.in_interrupt
        )

        # The set outranks the clear, so an edge which arrives in the cycle an instruction boundary
        # is passed is kept for the next boundary rather than dropped
        with m.If(int_edge & self.can_interrupt):
            m.d.sync += self.int_pending.eq(1)
        with m.Elif(self.boundary_passed):
            m.d.sync += self.int_pending.eq(0)


    def vector_addr(self, byte_sel: int) -> Value:
        return Cat(C(byte_sel, 1), self.vector_sel, C(0b1111111111111, 13))


    def cause_value(self, fault_kind: FaultKind, status: Value) -> Value:
        return Cat(fault_kind, C(0b00, 2), status)


    def advance(self, m: Module) -> None:
        with m.If(self.pc_carry):
            self.trap(m, FaultKind.PC_OVERFLOW, C(0x00, 8))
        with m.Else():
            m.d.sync += self.pc.eq(self.pc_next)
            m.next = CoreState.FETCH_LO


    def trap(self, m: Module, fault_kind: FaultKind, status: Value) -> None:
        m.d.sync += [
            self.cause.eq(self.cause_value(fault_kind, status)),
            self.vector_sel.eq(VectorSelect.FAULT)
        ]
        m.next = CoreState.TRAP_CAUSE


    def elaborate_vector_lo(self, m: Module) -> None:
        with m.State(CoreState.VECTOR_LO):
            m.d.comb += [
                self.mem_addr.eq(self.vector_addr(0)),
                self.mem_request.eq(1)
            ]
            with m.If(self.mem_ready):
                m.d.sync += self.inst[0:8].eq(self.mem_read_data)
                m.next = CoreState.VECTOR_HI


    def elaborate_vector_hi(self, m: Module) -> None:
        with m.State(CoreState.VECTOR_HI):
            m.d.comb += [
                self.mem_addr.eq(self.vector_addr(1)),
                self.mem_request.eq(1)
            ]
            with m.If(self.mem_ready):
                m.d.sync += self.pc.eq(Cat(self.inst[0:8], self.mem_read_data))
                m.next = CoreState.FETCH_LO


    def elaborate_fetch_lo(self, m: Module) -> None:
        with m.State(CoreState.FETCH_LO):
            with m.If(self.int_pending & self.can_interrupt):
                m.d.comb += self.boundary_passed.eq(1)
                m.d.sync += self.vector_sel.eq(VectorSelect.INTERRUPT)
                m.next = CoreState.TRAP_LINK
            with m.Elif(self.pc == 0xFFFF):
                m.d.comb += self.boundary_passed.eq(1)
                self.trap(m, FaultKind.PC_OVERFLOW, C(0x00, 8))
            with m.Else():
                m.d.comb += [
                    self.mem_addr.eq(self.pc),
                    self.mem_request.eq(1)
                ]
                with m.If(self.mem_ready):
                    m.d.comb += self.boundary_passed.eq(1)
                    m.d.sync += self.inst[0:8].eq(self.mem_read_data)
                    m.next = CoreState.FETCH_HI


    def elaborate_fetch_hi(self, m: Module) -> None:
        with m.State(CoreState.FETCH_HI):
            m.d.comb += [
                self.mem_addr.eq(self.pc + 1),
                self.mem_request.eq(1)
            ]
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

        with m.State(CoreState.DECODE):
            # Read for the system call and halt causes, which are the only register value this
            # state consumes
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
                        with m.If(decoder.is_syscall):
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
        with m.State(CoreState.EXECUTE_ALU):
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
        with m.State(CoreState.EXECUTE_SET):
            m.d.comb += [
                self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
                self.gp_reg_file.write_data.eq(self.decoder_unit.imm8),
                self.gp_reg_file.write_enable.eq(1)
            ]
            self.advance(m)


    def elaborate_execute_ld(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_LD):
            m.d.comb += [
                self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n2),
                self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2 + 1)
            ]
            with m.If(self.addr_fault):
                self.trap(m, FaultKind.ADDR_OVERFLOW, C(0x00, 8))
            with m.Else():
                m.d.comb += [
                    self.mem_addr.eq(self.eff_addr),
                    self.mem_request.eq(1)
                ]
                with m.If(self.mem_ready):
                    m.d.comb += [
                        self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
                        self.gp_reg_file.write_data.eq(self.mem_read_data),
                        self.gp_reg_file.write_enable.eq(1)
                    ]
                    self.advance(m)


    def elaborate_execute_ld_stk(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_LD_STK):
            m.d.comb += self.csr_reg_file.read_csr_reg.eq(CsrReg.SADDR)
            with m.If(self.addr_fault):
                self.trap(m, FaultKind.ADDR_OVERFLOW, C(0x00, 8))
            with m.Else():
                m.d.comb += [
                    self.mem_addr.eq(self.eff_addr),
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
        with m.State(CoreState.EXECUTE_ST_DATA):
            m.d.comb += self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1)
            m.d.sync += self.data_buf.eq(self.gp_reg_file.read_data_a)
            m.next = CoreState.EXECUTE_ST


    def elaborate_execute_st(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_ST):
            m.d.comb += [
                self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n2),
                self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2 + 1)
            ]
            with m.If(self.addr_fault):
                self.trap(m, FaultKind.ADDR_OVERFLOW, C(0x00, 8))
            with m.Else():
                m.d.comb += [
                    self.mem_addr.eq(self.eff_addr),
                    self.mem_write_data.eq(self.data_buf),
                    self.mem_write_enable.eq(1),
                    self.mem_request.eq(1)
                ]
                with m.If(self.mem_ready):
                    self.advance(m)


    def elaborate_execute_st_stk(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_ST_STK):
            m.d.comb += [
                self.csr_reg_file.read_csr_reg.eq(CsrReg.SADDR),
                self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1)
            ]
            with m.If(self.addr_fault):
                self.trap(m, FaultKind.ADDR_OVERFLOW, C(0x00, 8))
            with m.Else():
                m.d.comb += [
                    self.mem_addr.eq(self.csr_reg_file.read_data),
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
        with m.State(CoreState.EXECUTE_RSR_HI):
            m.d.comb += [
                self.csr_reg_file.read_csr_reg.eq(self.decoder_unit.csr),

                self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n1),
                self.gp_reg_file.write_data.eq(self.csr_reg_file.read_data[8:16]),
                self.gp_reg_file.write_enable.eq(1)
            ]
            m.next = CoreState.EXECUTE_RSR_LO


    def elaborate_execute_rsr_lo(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_RSR_LO):
            m.d.comb += [
                self.csr_reg_file.read_csr_reg.eq(self.decoder_unit.csr),

                self.gp_reg_file.write_gp_reg.eq(self.decoder_unit.n2),
                self.gp_reg_file.write_data.eq(self.csr_reg_file.read_data[0:8]),
                self.gp_reg_file.write_enable.eq(1)
            ]
            self.advance(m)


    def elaborate_execute_wsr(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_WSR):
            m.d.comb += [
                self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),
                self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2),

                self.csr_reg_file.write_csr_reg.eq(self.decoder_unit.csr),
                self.csr_reg_file.write_data.eq(self.base),
                self.csr_reg_file.write_enable.eq(1)
            ]
            self.advance(m)


    def elaborate_execute_branch(self, m: Module) -> None:
        with m.State(CoreState.EXECUTE_BRANCH):
            m.d.comb += [
                self.gp_reg_file.read_gp_reg_a.eq(self.decoder_unit.n1),
                self.gp_reg_file.read_gp_reg_b.eq(self.decoder_unit.n2)
            ]
            with m.If(self.cmp_unit.true):
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
        with m.State(CoreState.EXECUTE_JMP_CSR):
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
        with m.State(CoreState.EXECUTE_RETIR_HI):
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
        with m.State(CoreState.EXECUTE_RETIR_LO):
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
        with m.State(CoreState.EXECUTE_HALT):
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
        with m.State(CoreState.HALTED):
            m.d.comb += [
                self.csr_reg_file.read_csr_reg.eq(CsrReg.CAUSE),
                self.halted.eq(1),
                self.halt_code.eq(self.csr_reg_file.read_data[8:16])
            ]
            m.next = CoreState.HALTED


    def elaborate_trap_cause(self, m: Module) -> None:
        with m.State(CoreState.TRAP_CAUSE):
            m.d.comb += [
                self.csr_reg_file.write_csr_reg.eq(CsrReg.CAUSE),
                self.csr_reg_file.write_data.eq(self.cause),
                self.csr_reg_file.write_enable.eq(1)
            ]
            m.next = CoreState.TRAP_LINK


    def elaborate_trap_link(self, m: Module) -> None:
        with m.State(CoreState.TRAP_LINK):
            with m.Switch(self.vector_sel):
                with m.Case(VectorSelect.FAULT):
                    m.d.comb += [
                        self.csr_reg_file.write_csr_reg.eq(CsrReg.FAULT),
                        self.csr_reg_file.write_data.eq(self.pc),
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
        with m.State(CoreState.TRAP_ENTER):
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
            m.next = CoreState.VECTOR_LO

import MisaProof.Wf


-- Destination register binders are named rd after the architecture's operand names, which collides
-- with the GpReg.rd constructor
set_option linter.constructorNameAsVariable false


namespace MisaSpec


-- Taking a fault leaves the registers, memory, comparisons, and every special register other than
-- CAUSE, FAULT, and PRIVS unchanged
theorem fault_frame (q : State) (c : CsrCause) :
    (forall r : GpReg, (q.fault c).register r = q.register r)
      /\ (forall a : AddrWord, (q.fault c).memory a = q.memory a)
      /\ (forall f : CmpFlag, (q.fault c).check f = q.check f)
      /\ (q.fault c).special CsrReg.saddr = q.special CsrReg.saddr
      /\ (q.fault c).special CsrReg.raddr = q.special CsrReg.raddr
      /\ (q.fault c).special CsrReg.flags = q.special CsrReg.flags
      /\ (q.fault c).special CsrReg.extns = q.special CsrReg.extns
      /\ (q.fault c).special CsrReg.retsc = q.special CsrReg.retsc
      /\ (q.fault c).special CsrReg.retir = q.special CsrReg.retir := by
  repeat (any_goals apply And.intro)
  all_goals (intros; rfl)


-- Taking a fault does not change whether the processor is in a system call or interrupt context,
-- so a fault handler entered from an interrupt context leaves interrupts masked
theorem fault_preserves_context (q : State) (c : CsrCause) :
    (q.fault c).inSyscall = q.inSyscall
      /\ (q.fault c).inInterrupt = q.inInterrupt :=
  And.intro rfl rfl


-- Taking a fault records the program counter at the time of the fault in FAULT
theorem fault_records_pc (q : State) (c : CsrCause) :
    (q.fault c).special CsrReg.fault = q.pc := by
  simp [State.fault, State.writeSpecial, CsrReg.writeMask] <;> bv_decide


-- Taking a fault records the cause in CAUSE
theorem fault_sets_cause (q : State) (c : CsrCause) :
    (q.fault c).special CsrReg.cause = c.enc &&& CsrReg.cause.writeMask :=
  rfl


-- Taking a fault continues execution at the fault handler address in the reset vector table
theorem fault_jumps_to_vector (q : State) (c : CsrCause) :
    (q.fault c).pc = q.faultVector :=
  rfl


-- A successful HALT leaves FAULT unchanged
theorem halt_preserves_fault_reg (q : State) (rs : GpReg) (hu : q.userMode = false) :
    (executeHalt q rs).special CsrReg.fault = q.special CsrReg.fault := by
  simp [executeHalt, hu, State.writeSpecial, CsrReg.enc]


-- A successful SYSCALL leaves FAULT unchanged
theorem syscall_preserves_fault_reg (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) (hpc : q.pc < 0xFFFE#16) :
    (executeSyscall q rs).special CsrReg.fault = q.special CsrReg.fault := by
  simp [executeSyscall, hs, hi, linkAddr_of_lt q.pc hpc, State.writeSpecial, CsrReg.enc]


-- Below the end of the address space the program counter advances by two without recording a cause
theorem next_advances (q : State) (h : q.pc < 0xFFFE#16) :
    (State.next q).pc = q.pc + 2
      /\ (State.next q).special CsrReg.cause = q.special CsrReg.cause := by
  have hn : Not ((q.pc + 2) < q.pc) := by bv_decide
  simp only [State.next]
  split
  next hc => exact absurd hc hn
  next => exact And.intro rfl rfl


-- A sequential increment past the end of the address space raises a PC overflow
theorem next_overflows (q : State) (h : 0xFFFE#16 <= q.pc) :
    State.next q = q.fault CsrCause.pcOverflow := by
  have hn : (q.pc + 2) < q.pc := by bv_decide
  simp only [State.next]
  split
  next => rfl
  next hc => exact absurd hn hc


-- Below the end of the address space the program counter increment raises no fault, so it leaves
-- every special register unchanged, including the three a fault would record into
theorem next_special_of_lt (q : State) (c : CsrReg) (h : q.pc < 0xFFFE#16) :
    (State.next q).special c = q.special c := by
  have hn : Not ((q.pc + 2) < q.pc) := by bv_decide
  simp only [State.next]
  split
  next hc => exact absurd hc hn
  next => rfl


-- A program counter of 0xFFFF raises a PC overflow recording 0xFFFF, because the high byte of the
-- instruction word would be fetched from outside the address space
theorem execute_pc_max (q : State) (h : q.pc = 0xFFFF#16) :
    execute q = q.fault CsrCause.pcOverflow
      /\ (execute q).special CsrReg.fault = 0xFFFF#16 := by
  refine And.intro ?_ ?_
  next => simp [execute, h]
  next => simp [execute, h, fault_records_pc]


-- Outside 0xFFFF the address of the instruction word's high byte does not wrap, so the guard in
-- execute means fetch never assembles a word from both ends of the address space
theorem fetch_no_wrap (q : State) (h : Ne q.pc 0xFFFF#16) : q.pc < q.pc + 1 := by
  bv_decide


-- A taken JMP sets the program counter from its target and cannot raise a PC overflow
theorem jmp_taken_no_overflow (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = true) :
    (executeJmp q rs1 rs2 cmp).pc = (q.register rs1 ++ q.register rs2)
      /\ (executeJmp q rs1 rs2 cmp).special CsrReg.cause = q.special CsrReg.cause := by
  simp [executeJmp, h]


-- A taken JAL sets the program counter from its target and cannot raise a PC overflow
theorem jal_taken_no_overflow (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = true) (hpc : q.pc < 0xFFFE#16) :
    (executeJal q rs1 rs2 cmp).pc = (q.register rs1 ++ q.register rs2)
      /\ (executeJal q rs1 rs2 cmp).special CsrReg.cause = q.special CsrReg.cause := by
  simp [executeJal, h, linkAddr_of_lt q.pc hpc, State.writeSpecial, CsrReg.enc]


-- An instruction that completes before its sequential increment overflows keeps its effects, which
-- is the one exception to the fault frame
theorem overflow_after_write_retains_effect (q : State) (rd : GpReg) (imm : DataWord)
    (hrd : Ne rd GpReg.r0) (hpc : 0xFFFE#16 <= q.pc) :
    (executeSet q rd imm).register rd = imm
      /\ (executeSet q rd imm).special CsrReg.fault = q.pc := by
  have hq : (q.writeReg rd imm).pc = q.pc := by cases rd <;> rfl
  have h2 : 0xFFFE#16 <= (q.writeReg rd imm).pc := by rw [hq]; exact hpc
  refine And.intro ?_ ?_
  next =>
    simp only [executeSet, State.next_register]
    exact State.writeReg_self q rd imm hrd
  next =>
    simp only [executeSet]
    rw [next_overflows _ h2, fault_records_pc, hq]


-- A load or store immediate ranges from -8 to 7
theorem halfWord_toInt_range (imm : HalfWord) : -8 <= imm.toInt /\ imm.toInt <= 7 :=
  of_decide_eq_true (forall_halfWord
    (fun m => decide (-8 <= m.toInt /\ m.toInt <= 7)) (by decide) imm)


-- Sign extending a load or store immediate to seventeen bits adds the modulus when the immediate
-- is negative
theorem halfWord_signExtend_toNat (imm : HalfWord) :
    ((imm.signExtend 17).toNat : Int) = imm.toInt + (if imm.toInt < 0 then 131072 else 0) :=
  of_decide_eq_true (forall_halfWord
    (fun m => decide (((m.signExtend 17).toNat : Int)
      = m.toInt + (if m.toInt < 0 then 131072 else 0))) (by decide) imm)


-- The seventeen bit sum behind an effective address is the integer sum of the base and the sign
-- extended immediate, wrapped only when that sum is negative
theorem effAddr_wide (base : AddrWord) (imm : HalfWord) :
    (((base.setWidth 17) + (imm.signExtend 17)).toNat : Int)
      = (base.toNat : Int) + imm.toInt
        + (if (base.toNat : Int) + imm.toInt < 0 then 131072 else 0) := by
  have hb : base.toNat < 65536 := base.isLt
  have hi := halfWord_toInt_range imm
  have hs := halfWord_signExtend_toNat imm
  simp only [BitVec.toNat_add, BitVec.toNat_setWidth]
  by_cases hi0 : imm.toInt < 0
  all_goals simp [hi0] at hs
  all_goals by_cases hneg : (base.toNat : Int) + imm.toInt < 0
  all_goals simp [hneg]
  all_goals omega


-- The carry out of an effective address computation is exactly the integer sum leaving the address
-- range, which is why the computation never wraps
theorem effAddr_carry (base : AddrWord) (imm : HalfWord) :
    ((base.setWidth 17) + (imm.signExtend 17)).getLsbD 16
      = decide ((base.toNat : Int) + imm.toInt < 0
                \/ 65535 < (base.toNat : Int) + imm.toInt) := by
  have hb : base.toNat < 65536 := base.isLt
  have hi := halfWord_toInt_range imm
  have hw := effAddr_wide base imm
  rw [bit16_eq_ge]
  simp only [decide_eq_decide]
  by_cases hneg : (base.toNat : Int) + imm.toInt < 0
  all_goals simp only [hneg] at hw
  all_goals omega


-- An effective address is computed exactly when the base plus the sign extended immediate stays
-- within the sixteen bit address range, and then it is that sum
theorem effAddr_eq_some (base : AddrWord) (imm : HalfWord) (a : AddrWord) :
    effAddr base imm = some a
      <-> (0 <= (base.toNat : Int) + imm.toInt
           /\ (base.toNat : Int) + imm.toInt <= 65535
           /\ (a.toNat : Int) = (base.toNat : Int) + imm.toInt) := by
  have hb : base.toNat < 65536 := base.isLt
  have ha : a.toNat < 65536 := a.isLt
  have hi := halfWord_toInt_range imm
  have hw := effAddr_wide base imm
  simp only [effAddr, effAddr_carry]
  split
  next hc =>
    have hc' : (base.toNat : Int) + imm.toInt < 0 \/ 65535 < (base.toNat : Int) + imm.toInt := by
      simpa using hc
    simp <;> omega
  next hc =>
    have hc' : Not ((base.toNat : Int) + imm.toInt < 0
        \/ 65535 < (base.toNat : Int) + imm.toInt) := by simpa using hc
    have hneg : Not ((base.toNat : Int) + imm.toInt < 0) := by omega
    simp [hneg] at hw
    simp only [Option.some.injEq]
    constructor
    next =>
      intro heq
      have ht := congrArg BitVec.toNat heq
      simp [BitVec.toNat_setWidth] at ht
      omega
    next =>
      intro hr
      apply BitVec.eq_of_toNat_eq
      simp [BitVec.toNat_setWidth]
      omega


-- An effective address computation fails exactly when the base plus the sign extended immediate
-- would leave the sixteen bit address range, so it never wraps
theorem effAddr_eq_none (base : AddrWord) (imm : HalfWord) :
    effAddr base imm = none
      <-> ((base.toNat : Int) + imm.toInt < 0 \/ 65535 < (base.toNat : Int) + imm.toInt) := by
  simp only [effAddr, effAddr_carry]
  split
  next hc => simpa using hc
  next hc => simpa using hc


-- An address lies outside the reset vector table exactly when it is below the base of the table,
-- since the table runs to the end of the address space
theorem inVectorTable_eq_false_iff (addr : AddrWord) :
    inVectorTable addr = false <-> addr < 0xFFF0#16 := by
  simp only [inVectorTable, decide_eq_false_iff_not]
  bv_decide


-- Advancing the program counter leaves SADDR unchanged, whether or not it overflows into a fault
theorem next_saddr (q : State) :
    (State.next q).special CsrReg.saddr = q.special CsrReg.saddr := by
  simp only [State.next]
  split
  next => exact (fault_frame q CsrCause.pcOverflow).2.2.2.1
  next => rfl


-- Advancing the program counter leaves memory unchanged
theorem next_memory (q : State) (a : AddrWord) : (State.next q).memory a = q.memory a := by
  simp only [State.next]
  split
  next => exact (fault_frame q CsrCause.pcOverflow).2.1 a
  next => rfl


-- A stack LD whose updated SADDR would overflow leaves SADDR, the registers, and memory unchanged
theorem ld_overflow_no_effect (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (h : effAddr (q.special CsrReg.saddr) imm = none) (hrsb : rsb = GpReg.rscratch1) :
    let q' := executeLd q rd rsb imm
    q'.special CsrReg.saddr = q.special CsrReg.saddr
      /\ (forall r : GpReg, q'.register r = q.register r)
      /\ (forall a : AddrWord, q'.memory a = q.memory a) := by
  subst hrsb
  simp [executeLd, h, fault_frame]


-- A stack ST whose updated SADDR would overflow leaves SADDR and memory unchanged, so it performs
-- no memory access
theorem st_overflow_no_effect (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (h : effAddr (q.special CsrReg.saddr) imm = none) (hrsb : rsb = GpReg.rscratch1) :
    let q' := executeSt q rd rsb imm
    q'.special CsrReg.saddr = q.special CsrReg.saddr
      /\ (forall a : AddrWord, q'.memory a = q.memory a) := by
  subst hrsb
  simp [executeSt, h, fault_frame]


-- A register-based LD whose effective address would overflow leaves every register unchanged
theorem ld_base_overflow_no_effect (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (hrsb : Ne rsb GpReg.rscratch1)
    (h : effAddr (q.register rsb ++ q.register ((GpReg.dec (rsb.enc + 1)).getD GpReg.r0)) imm
           = none) :
    forall r : GpReg, (executeLd q rd rsb imm).register r = q.register r := by
  intro r
  cases rsb
  case rscratch1 => exact absurd rfl hrsb
  all_goals simp_all [executeLd, fault_frame]


-- The low half of a load or store base pair is the register after the one the instruction names,
-- and the fallback to R0 is never taken
theorem ld_low_register (rsb : GpReg) (h : Ne rsb GpReg.rscratch1) :
    ((GpReg.dec (rsb.enc + 1)).getD GpReg.r0).enc = rsb.enc + 1 := by
  cases rsb
  case rscratch1 => exact absurd rfl h
  all_goals decide


-- A register-based LD reads from the base pair plus the sign extended immediate
theorem ld_base_address (q : State) (rd : GpReg) (rsb : GpReg) (lo : GpReg) (imm : HalfWord)
    (a : AddrWord)
    (hrsb : Ne rsb GpReg.rscratch1) (hrd : Ne rd GpReg.r0)
    (hlo : lo = (GpReg.dec (rsb.enc + 1)).getD GpReg.r0)
    (hok : q.accessFault AccessKind.read a = none)
    (h : effAddr (q.register rsb ++ q.register lo) imm = some a) :
    (executeLd q rd rsb imm).register rd = q.memory a := by
  subst hlo
  cases rsb
  case rscratch1 => exact absurd rfl hrsb
  all_goals
    simp only [executeLd, h, hok, State.next_register]
    exact State.writeReg_self q rd (q.memory a) hrd


-- A stack LD updates SADDR and then reads from the new stack address
theorem ld_stack_order (q : State) (rd : GpReg) (imm : HalfWord) (s : AddrWord)
    (hrd : Ne rd GpReg.r0) (hok : q.accessFault AccessKind.read s = none)
    (h : effAddr (q.special CsrReg.saddr) imm = some s) :
    let q' := executeLd q rd GpReg.rscratch1 imm
    q'.special CsrReg.saddr = s /\ q'.register rd = q.memory s := by
  simp only [executeLd, h, hok]
  refine And.intro ?_ ?_
  next =>
    rw [next_saddr, State.writeReg_special,
        State.writeSpecial_self q CsrReg.saddr s (by decide)]
    simp [CsrReg.writeMask] <;> bv_decide
  next =>
    rw [State.next_register]
    exact State.writeReg_self _ rd _ hrd


-- A stack ST outside the reset vector table writes to the old stack address and then updates SADDR
theorem st_stack_order (q : State) (rd : GpReg) (imm : HalfWord) (s : AddrWord)
    (hok : q.accessFault AccessKind.write (q.special CsrReg.saddr) = none)
    (h : effAddr (q.special CsrReg.saddr) imm = some s) :
    let q' := executeSt q rd GpReg.rscratch1 imm
    q'.memory (q.special CsrReg.saddr) = q.register rd /\ q'.special CsrReg.saddr = s := by
  simp only [executeSt, h, hok]
  refine And.intro ?_ ?_
  next =>
    rw [next_memory]
    simp [State.writeSpecial, State.writeMem, CsrReg.enc]
  next =>
    rw [next_saddr, State.writeSpecial_self _ CsrReg.saddr s (by decide)]
    simp [CsrReg.writeMask] <;> bv_decide


-- A ST leaves every register unchanged
theorem st_preserves_registers (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (r : GpReg) :
    (executeSt q rd rsb imm).register r = q.register r := by
  simp only [executeSt]
  repeat (any_goals split)
  all_goals first
    | exact (fault_frame q _).1 r
    | simp [State.next_register, State.writeSpecial_register, State.writeMem]


-- A stack ST leaves every address other than the stack address unchanged
theorem st_preserves_memory_other (q : State) (rd : GpReg) (imm : HalfWord) (a : AddrWord)
    (h : Ne a (q.special CsrReg.saddr)) :
    (executeSt q rd GpReg.rscratch1 imm).memory a = q.memory a := by
  simp only [executeSt]
  split
  next => exact (fault_frame q _).2.1 a
  next =>
    split
    next => exact (fault_frame q _).2.1 a
    next => rw [next_memory, State.writeSpecial_memory, State.writeMem_other q _ a _ h]


-- A LD leaves memory unchanged
theorem ld_preserves_memory (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (a : AddrWord) :
    (executeLd q rd rsb imm).memory a = q.memory a := by
  simp only [executeLd]
  repeat (any_goals split)
  all_goals first
    | exact (fault_frame q _).2.1 a
    | simp [next_memory, State.writeReg_memory, State.writeSpecial_memory]


-- A LD leaves every register other than its destination unchanged
theorem ld_preserves_registers_other (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (r : GpReg) (h : Ne r rd) :
    (executeLd q rd rsb imm).register r = q.register r := by
  simp only [executeLd]
  repeat (any_goals split)
  all_goals first
    | exact (fault_frame q _).1 r
    | simp [State.next_register, State.writeReg_other _ rd r _ h, State.writeSpecial_register]


-- A HALT leaves FLAGS unchanged
theorem halt_preserves_flags (q : State) (rs : GpReg) :
    (executeHalt q rs).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeHalt]
  split
  all_goals rfl


-- A LD leaves FLAGS unchanged
theorem ld_preserves_flags (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord) :
    (executeLd q rd rsb imm).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeLd]
  repeat (any_goals split)
  all_goals first
    | rfl
    | simp [State.next_flags, State.writeReg_special, State.writeSpecial, CsrReg.enc]


-- A ST leaves FLAGS unchanged
theorem st_preserves_flags (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord) :
    (executeSt q rd rsb imm).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeSt]
  repeat (any_goals split)
  all_goals first
    | rfl
    | simp [State.next_flags, State.writeSpecial, State.writeMem, CsrReg.enc]


-- A RSR leaves FLAGS unchanged, even when FLAGS is the register it reads
theorem rsr_preserves_flags (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg) :
    (executeRsr q rs1 rs2 csr).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeRsr]
  split
  next => rfl
  next => simp [State.next_flags, State.writeReg_special]


-- A JAL leaves FLAGS unchanged
theorem jal_preserves_flags (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag) :
    (executeJal q rs1 rs2 cmp).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeJal]
  split
  next =>
    split
    next => exact (fault_frame q _).2.2.2.2.2.1
    next => simp [State.writeSpecial, CsrReg.enc]
  next => exact State.next_flags q


-- A JMP leaves FLAGS unchanged
theorem jmp_preserves_flags (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag) :
    (executeJmp q rs1 rs2 cmp).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeJmp]
  split
  next => rfl
  next => exact State.next_flags q


-- A JMP on a special register other than RETIR leaves FLAGS unchanged, RETIR being the one target
-- that restores the saved flags
theorem jmpCsr_preserves_flags (q : State) (csr : CsrReg) (cmp : CmpFlag)
    (h : Ne csr CsrReg.retir) :
    (executeJmpCsr q csr cmp).special CsrReg.flags = q.special CsrReg.flags := by
  cases csr
  case retir => exact absurd rfl h
  all_goals
    simp only [executeJmpCsr]
    repeat (any_goals split)
    all_goals first
      | rfl
      | exact State.next_flags q
      | simp [State.writeSpecial, CsrReg.enc]


-- A WSR only reaches FLAGS when FLAGS is the register it names, never through the ALU output
theorem wsr_preserves_flags (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg)
    (h : Ne csr CsrReg.flags) :
    (executeWsr q rs1 rs2 csr).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [executeWsr]
  split
  next => rfl
  next =>
    rw [State.next_flags]
    exact State.writeSpecial_other q csr CsrReg.flags _ (Ne.symm h)


-- A fetch that cannot complete raises the fetch's own fault, and nothing is decoded
theorem decodeAndExecute_fetch_fault (q : State) (cause : CsrCause)
    (hf : fetch q = Except.error cause) : decodeAndExecute q = q.fault cause := by
  simp [decodeAndExecute, hf]


-- An instruction word that fails to decode raises an illegal instruction fault
theorem decodeAndExecute_illegal (q : State) (w : InstWord) (hf : fetch q = Except.ok w)
    (h : Inst.decUnder w (CsrExtns.dec (q.special CsrReg.extns)) = none) :
    decodeAndExecute q = q.fault CsrCause.illegalInstruction := by
  simp [decodeAndExecute, hf, h]


-- An instruction word that does not raise an illegal instruction fault is the encoding of some
-- available instruction
theorem decodeAndExecute_legal_iff (q : State) (w : InstWord) (hf : fetch q = Except.ok w) :
    Ne (decodeAndExecute q) (q.fault CsrCause.illegalInstruction)
      -> Exists fun i : Inst =>
           Inst.decUnder w (CsrExtns.dec (q.special CsrReg.extns)) = some i
             /\ i.enc = w := by
  intro hne
  cases hd : Inst.decUnder w (CsrExtns.dec (q.special CsrReg.extns)) with
  | none   => exact absurd (decodeAndExecute_illegal q w hf hd) hne
  | some i =>
    have hdec : Inst.dec w = some i := by
      cases hx : Inst.dec w with
      | none   => simp [Inst.decUnder, hx] at hd
      | some j =>
        simp only [Inst.decUnder, hx] at hd
        split at hd
        next => exact hd
        next => simp at hd
    refine Exists.intro i (And.intro ?_ (Inst.enc_dec w i hdec))
    first | exact hd | rfl


end MisaSpec

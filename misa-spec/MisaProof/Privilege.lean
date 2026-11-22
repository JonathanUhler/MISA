import MisaProof.Vector


namespace MisaSpec


-- Writing PRIVS back to the value it already holds is the identity, since a well formed state
-- holds no bit the write mask drops
theorem privs_mask_self (q : State) (hwf : q.WF) :
    q.special CsrReg.privs &&& CsrReg.privs.writeMask = q.special CsrReg.privs := by
  have hm := hwf.csrMasked CsrReg.privs
  simp only [CsrReg.writeMask] at hm
  simp only [CsrReg.writeMask]
  bv_decide


-- An automatic privilege change writes the ring bit it names while the Privilege Extension is
-- enabled
theorem privsUpdate_enabled (q : State) (v : SpecWord)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = true) :
    q.privsUpdate v = v := by
  unfold State.privsUpdate
  split
  next => rfl
  next hc => simp [h] at hc


-- An automatic privilege change writes the current ring bit back while the Privilege Extension is
-- disabled, whatever ring bit it would otherwise have named
theorem privsUpdate_disabled (q : State) (v : SpecWord)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    q.privsUpdate v = q.special CsrReg.privs := by
  unfold State.privsUpdate
  split
  next hc => simp [h] at hc
  next => rfl


-- A state which took its PRIVS from an automatic clear is in Machine Mode: the clear lands while
-- the Privilege Extension is enabled, and the level is Machine Mode whatever the ring bit holds
-- while it is disabled
theorem userMode_of_privsUpdate_zero (q : State) (q' : State)
    (he : q'.special CsrReg.extns = q.special CsrReg.extns)
    (hp : q'.special CsrReg.privs = q.privsUpdate 0x0000 &&& CsrReg.privs.writeMask) :
    q'.userMode = false := by
  simp only [State.userMode, he, hp]
  unfold State.privsUpdate
  split
  next hc => simp [hc, CsrReg.writeMask]
  next hc =>
    have hf : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false := by simpa using hc
    simp [hf]


-- Taking a fault leaves the processor in Machine Mode
theorem faultAt_enters_machine_mode (q : State) (c : CsrCause) (loc : AddrWord) :
    (q.faultAt c loc).userMode = false := by
  refine userMode_of_privsUpdate_zero q _ ?_ ?_
  all_goals (simp only [State.faultAt]; split)
  all_goals simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]


theorem fault_enters_machine_mode (q : State) (c : CsrCause) : (q.fault c).userMode = false :=
  faultAt_enters_machine_mode q c q.pc


-- A SYSCALL leaves the processor in Machine Mode whichever way it ends: entering its handler, or
-- faulting on a nested call, on a link overflow, or on a syscall vector it cannot read
theorem syscall_enters_machine_mode (q : State) (rs : GpReg) :
    (executeSyscall q rs).userMode = false := by
  simp only [executeSyscall]
  -- a nested call, then a link overflow, then a syscall vector which cannot be read
  split
  next => exact faultAt_enters_machine_mode _ _ _
  next =>
    split
    next => exact faultAt_enters_machine_mode _ _ _
    next =>
      split
      next => exact faultAt_enters_machine_mode _ _ _
      next =>
        refine userMode_of_privsUpdate_zero q _ ?_ ?_
        all_goals simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]


-- Taking an interrupt leaves the processor in Machine Mode, whether it enters its handler or faults
-- on an interrupt vector it cannot read
theorem interrupt_enters_machine_mode (q : State) : q.interrupt.userMode = false := by
  simp only [State.interrupt]
  split
  next =>
    refine userMode_of_privsUpdate_zero q _ ?_ ?_
    all_goals simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]
  next => exact faultAt_enters_machine_mode _ _ _


-- Taking a fault drops the ring bit through the automatic update
theorem faultAt_privs_update (q : State) (c : CsrCause) (loc : AddrWord) :
    (q.faultAt c loc).special CsrReg.privs
      = q.privsUpdate 0x0000 &&& CsrReg.privs.writeMask := by
  simp only [State.faultAt]
  split
  all_goals simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]


theorem fault_privs_update (q : State) (c : CsrCause) :
    (q.fault c).special CsrReg.privs = q.privsUpdate 0x0000 &&& CsrReg.privs.writeMask :=
  faultAt_privs_update q c q.pc


-- Taking a fault leaves PRIVS alone while the Privilege Extension is disabled, since a disabled
-- extension keeps the contents of the registers it defines until it is re-enabled. The processor
-- is already in Machine Mode by the definition of the privilege level, so there is nothing for the
-- trap to drop to
theorem fault_preserves_privs_when_extension_disabled (q : State) (hwf : q.WF) (c : CsrCause)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    (q.fault c).special CsrReg.privs = q.special CsrReg.privs := by
  rw [fault_privs_update q c, privsUpdate_disabled q 0x0000 h]
  exact privs_mask_self q hwf


-- Taking an interrupt leaves PRIVS alone while the Privilege Extension is disabled
theorem interrupt_preserves_privs_when_extension_disabled (q : State) (hwf : q.WF)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    q.interrupt.special CsrReg.privs = q.special CsrReg.privs := by
  have hv : q.interrupt.special CsrReg.privs
      = q.privsUpdate 0x0000 &&& CsrReg.privs.writeMask := by
    simp only [State.interrupt]
    split
    next => simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]
    next => exact faultAt_privs_update _ _ _
  rw [hv, privsUpdate_disabled q 0x0000 h]
  exact privs_mask_self q hwf


-- A SYSCALL leaves PRIVS alone while the Privilege Extension is disabled
theorem syscall_preserves_privs_when_extension_disabled (q : State) (hwf : q.WF) (rs : GpReg)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    (executeSyscall q rs).special CsrReg.privs = q.special CsrReg.privs := by
  have hv : (executeSyscall q rs).special CsrReg.privs
      = q.privsUpdate 0x0000 &&& CsrReg.privs.writeMask := by
    simp only [executeSyscall]
    split
    next => exact faultAt_privs_update _ _ _
    next =>
      split
      next => exact faultAt_privs_update _ _ _
      next =>
        split
        next => exact faultAt_privs_update _ _ _
        next => simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]
  rw [hv, privsUpdate_disabled q 0x0000 h]
  exact privs_mask_self q hwf


-- A special register write that names neither PRIVS nor EXTNS leaves the privilege level unchanged
theorem writeSpecial_userMode (q : State) (c : CsrReg) (v : SpecWord)
    (hp : Ne c CsrReg.privs) (he : Ne c CsrReg.extns) :
    (q.writeSpecial c v).userMode = q.userMode := by
  simp only [State.userMode]
  rw [State.writeSpecial_other q c CsrReg.extns v (Ne.symm he),
      State.writeSpecial_other q c CsrReg.privs v (Ne.symm hp)]


-- A WSR that User Mode is permitted to perform names neither PRIVS nor EXTNS, so it cannot raise
-- the privilege level
theorem wsr_user_preserves_userMode (q : State) (v : SpecWord) (csr : CsrReg)
    (hu : q.userMode = true) (hg : Not ((q.userMode && !csr.userWritable) = true)) :
    (q.writeSpecial csr v).userMode = q.userMode := by
  have hw : csr.userWritable = true := by
    rw [hu] at hg
    simp at hg
    exact hg
  cases csr
  case saddr => exact writeSpecial_userMode q CsrReg.saddr v (by decide) (by decide)
  case raddr => exact writeSpecial_userMode q CsrReg.raddr v (by decide) (by decide)
  case flags => exact writeSpecial_userMode q CsrReg.flags v (by decide) (by decide)
  all_goals simp [CsrReg.userWritable] at hw


-- Control has reached the fault handler at the given address, or the fault could not read that
-- address and the processor halted, which is the only other way a fault can end. Both outcomes
-- leave Machine Mode behind, so both are outside the reach of the User Mode program that faulted.
def State.inFaultHandlerAt (q : State) (target : AddrWord) : Prop :=
  q.pc = target \/ q.halted = true


-- The same, measured against the fault handler address the state itself reads
def State.inFaultHandler (q : State) : Prop :=
  q.inFaultHandlerAt q.faultVector


-- A fault either lands on the fault handler address as the faulted state reads it, or halts
theorem faultAt_enters_fault_handler (q : State) (c : CsrCause) (loc : AddrWord) :
    (q.faultAt c loc).inFaultHandler := by
  simp only [State.inFaultHandler, State.inFaultHandlerAt, State.faultVector]
  rw [(faultAt_frame q c loc).2.1 0xFFFD, (faultAt_frame q c loc).2.1 0xFFFC]
  exact faultAt_enters_handler_or_halts q c loc


theorem fault_enters_fault_handler (q : State) (c : CsrCause) : (q.fault c).inFaultHandler :=
  faultAt_enters_fault_handler q c q.pc


-- An interrupt whose handler address can be read lands on that address as the interrupted state
-- reads it
theorem interrupt_pc_eq_intVector (q : State) (hv : q.intVectorReadable) :
    q.interrupt.pc = q.interrupt.intVector := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  simp only [State.interrupt, h, State.intVector, State.memory]
  simp [State.writeSpecial, State.setCsr]


-- An interrupt is contained the same way a fault is: it lands on the interrupt handler address, or
-- the vector could not be read and it is the fault handler, or a halt, that it reaches instead
theorem interrupt_contained (q : State) :
    q.interrupt.pc = q.interrupt.intVector \/ q.interrupt.inFaultHandler := by
  cases h : q.readVector 0xFFF8 with
  | error c =>
    simp only [State.interrupt, h]
    exact Or.inr (faultAt_enters_fault_handler _ _ _)
  | ok t     =>
    have ha : (0xFFF8 : AddrWord) + 1 = 0xFFF9 := by bv_decide
    have ht := readVector_ok_value q 0xFFF8 t h
    rw [ha] at ht
    have hiv : q.intVector = t := ht
    refine Or.inl (interrupt_pc_eq_intVector q ?_)
    show q.readVector 0xFFF8 = Except.ok q.intVector
    rw [hiv]
    exact h


-- The program counter increment either keeps the privilege level or raises a PC overflow, which
-- lands in the fault handler or halts
theorem next_contained (s : State) (h : s.userMode = true) :
    (State.next s).userMode = true \/ (State.next s).inFaultHandler := by
  simp only [State.next]
  split
  next => exact Or.inr (fault_enters_fault_handler s CsrCause.pcOverflow)
  next => exact Or.inl h


-- Reaching the program counter increment from a state with s's privilege level is contained
theorem next_contained3 (s : State) (t : State) (hu : s.userMode = true)
    (h : t.userMode = s.userMode) :
    (State.next t).userMode = true
      \/ (State.next t).inFaultHandler
      \/ (State.next t).pc = (State.next t).syscallVector := by
  refine ((next_contained t ?_).imp id Or.inl)
  rw [h]
  exact hu


-- A JMP naming a special register that User Mode is permitted to read names neither RETSC nor
-- RETIR, since neither is user readable
theorem jmpCsr_user_readable (q : State) (csr : CsrReg) (hu : q.userMode = true)
    (hg : Not ((q.userMode && !csr.userReadable) = true)) : csr.userReadable = true := by
  rw [hu] at hg
  simp at hg
  exact hg


-- Executing an instruction in User Mode either leaves the privilege level alone or lands in the
-- fault or system call handler, so no instruction available to User Mode raises its own privilege.
-- A fault which cannot read its own handler address halts instead, which the fault disjunct covers
-- and which is no escape either. Each instruction is unfolded inside its own branch: unfolding all
-- eighteen into one goal before splitting is too large to elaborate.
theorem decodeAndExecute_contained (s : State) (hu : s.userMode = true) :
    (decodeAndExecute s).userMode = true
      \/ (decodeAndExecute s).inFaultHandler
      \/ (decodeAndExecute s).pc = (decodeAndExecute s).syscallVector := by
  simp only [decodeAndExecute]
  split
  -- the fetch itself faults
  next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
  next =>
  split
  -- the instruction word does not decode
  next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
  next =>
    split
    -- HALT is privileged, so User Mode only ever faults on it
    next =>
      simp only [executeHalt, hu]
      exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
    -- the eight register and flag writing instructions, then SET
    next =>
      simp only [executeAdd]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeAdc]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeSub]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeSbb]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeAnd]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeOr]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeXor]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeRrc]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special, CsrReg.enc]
    next =>
      simp only [executeSet]
      refine next_contained3 s _ hu ?_
      simp [State.userMode, State.writeReg_special]
    -- LD and ST, each with a stack and a register base form
    next =>
      simp only [executeLd]
      repeat (any_goals split)
      all_goals first
        | exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
        | (refine next_contained3 s _ hu ?_
           simp [State.userMode, State.writeReg_special, State.writeSpecial_other])
    next =>
      simp only [executeSt]
      repeat (any_goals split)
      all_goals first
        | exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
        | (refine next_contained3 s _ hu ?_
           simp [State.userMode, State.writeSpecial_other, State.writeMem])
    -- RSR only reads, and WSR can only name a register User Mode may write
    next =>
      simp only [executeRsr]
      split
      next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
      next =>
        refine next_contained3 s _ hu ?_
        simp [State.userMode, State.writeReg_special]
    next =>
      simp only [executeWsr]
      split
      next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
      next hg =>
        refine next_contained3 s _ hu ?_
        exact wsr_user_preserves_userMode s _ _ hu hg
    -- JAL and JMP reach either their target or the increment
    next =>
      simp only [executeJal]
      split
      next =>
        split
        -- the link register computation overflows, so the JAL faults instead of branching
        next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
        next link _ =>
          refine Or.inl ?_
          rw [<- hu]
          exact writeSpecial_userMode s CsrReg.raddr link (by decide) (by decide)
      next =>
        refine next_contained3 s _ hu ?_
        rfl
    next =>
      simp only [executeJmp]
      split
      next =>
        refine Or.inl ?_
        exact hu
      next =>
        refine next_contained3 s _ hu ?_
        rfl
    -- the special register variant of JMP, whose two context targets User Mode cannot read
    next =>
      simp only [executeJmpCsr]
      split
      next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
      next hg =>
        have hr := jmpCsr_user_readable s _ hu hg
        split
        next => simp [CsrReg.userReadable] at hr
        next => simp [CsrReg.userReadable] at hr
        next =>
          split
          next =>
            refine Or.inl ?_
            exact hu
          next =>
            refine next_contained3 s _ hu ?_
            rfl
    -- SYSCALL either faults on a nested call or lands in its handler
    next =>
      simp only [executeSyscall]
      split
      next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
      next =>
        split
        -- the link register computation overflows, so the SYSCALL faults instead of entering
        next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
        next =>
          split
          -- the syscall handler address cannot be read, so the SYSCALL faults instead of entering
          next => exact Or.inr (Or.inl (faultAt_enters_fault_handler _ _ _))
          next t hok =>
            have ha : (0xFFFA : AddrWord) + 1 = 0xFFFB := by bv_decide
            have ht := readVector_ok_value s 0xFFFA t hok
            rw [ha] at ht
            refine Or.inr (Or.inr ?_)
            simp only [State.syscallVector, State.writeSpecial, State.special, State.setCsr]
            exact ht.symm


-- Fetching and executing in User Mode either leaves the privilege level alone or lands in the
-- fault or system call handler, or halts on a fault handler address it cannot read
theorem execute_contained (s : State) (hu : s.userMode = true) :
    (execute s).userMode = true
      \/ (execute s).inFaultHandler
      \/ (execute s).pc = (execute s).syscallVector := by
  simp only [execute]
  split
  next => exact Or.inr (Or.inl (fault_enters_fault_handler _ _))
  next => exact decodeAndExecute_contained s hu


-- A step taken in User Mode either stays in User Mode, ends up in one of the three trap handlers,
-- or halts because a fault could not read its handler address, so User Mode cannot raise its own
-- privilege. The handler it lands in is the one the program started with, because a store into the
-- reset vector table faults instead of writing; without that guarantee a User Mode store could
-- repoint a vector and the trap would then enter Machine Mode at an address the program chose.
theorem user_cannot_escalate (q : State) (_hwf : q.WF) (hh : q.halted = false)
    (hu : q.userMode = true) :
    (step q).userMode = true
      \/ (step q).inFaultHandlerAt q.faultVector
      \/ (step q).pc = q.syscallVector
      \/ (step q).pc = q.intVector := by
  -- the vector table the step lands on is the one the program started with
  have hvt := step_preserves_vector_table q
  have hfv : (step q).faultVector = q.faultVector := hvt.1
  have hsv : (step q).syscallVector = q.syscallVector := hvt.2.1
  have hiv : (step q).intVector = q.intVector := hvt.2.2.1
  -- the step lands in whichever handler the state it reaches reads, which the vector table result
  -- above ties back to the table the program started with
  have hpost : (step q).userMode = true
      \/ (step q).inFaultHandlerAt (step q).faultVector
      \/ (step q).pc = (step q).syscallVector
      \/ (step q).pc = (step q).intVector := by
    simp only [step, hh]
    split
    next hc => simp at hc
    next =>
      split
      next =>
        cases interrupt_contained q with
        | inl hiv' => exact Or.inr (Or.inr (Or.inr hiv'))
        | inr hfh  => exact Or.inr (Or.inl hfh)
      next =>
        refine Or.imp id (fun h => Or.imp id Or.inl h) (execute_contained _ ?_)
        exact hu
  rw [hfv, hsv, hiv] at hpost
  exact hpost


-- The special registers readable from User Mode are SADDR, RADDR, FLAGS, and PRIVS
theorem userReadable_table (csr : CsrReg) :
    csr.userReadable
      = (decide (csr = CsrReg.saddr) || decide (csr = CsrReg.raddr)
         || decide (csr = CsrReg.flags) || decide (csr = CsrReg.privs)) := by
  cases csr <;> decide


-- The special registers writable from User Mode are SADDR, RADDR, and FLAGS
theorem userWritable_table (csr : CsrReg) :
    csr.userWritable
      = (decide (csr = CsrReg.saddr) || decide (csr = CsrReg.raddr)
         || decide (csr = CsrReg.flags)) := by
  cases csr <;> decide


-- Every special register writable from User Mode is also readable from User Mode
theorem userWritable_subset_userReadable (csr : CsrReg) (h : csr.userWritable = true) :
    csr.userReadable = true := by
  cases csr
  case saddr => rfl
  case raddr => rfl
  case flags => rfl
  all_goals simp [CsrReg.userWritable] at h


-- PRIVS is readable but not writable from User Mode
theorem privs_readable_not_writable :
    CsrReg.privs.userReadable = true /\ CsrReg.privs.userWritable = false :=
  And.intro rfl rfl


-- EXTNS is not writable from User Mode, so User Mode cannot disable the Privilege Extension to
-- escape it
theorem extns_not_userWritable : CsrReg.extns.userWritable = false :=
  rfl


-- A JMP to RETSC from User Mode raises a privileged instruction fault, which takes precedence over
-- both of the control event faults that target could otherwise raise
theorem jmpCsr_privilege_precedence_retsc (q : State) (cmp : CmpFlag) (hu : q.userMode = true) :
    executeJmpCsr q CsrReg.retsc cmp = q.fault CsrCause.privilegedInstruction := by
  simp [executeJmpCsr, hu, CsrReg.userReadable]


-- A JMP to RETIR from User Mode raises a privileged instruction fault, which takes precedence over
-- both of the control event faults that target could otherwise raise
theorem jmpCsr_privilege_precedence_retir (q : State) (cmp : CmpFlag) (hu : q.userMode = true) :
    executeJmpCsr q CsrReg.retir cmp = q.fault CsrCause.privilegedInstruction := by
  simp [executeJmpCsr, hu, CsrReg.userReadable]


-- A JMP to RETSC or RETIR with a comparison other than ALWAYS raises a conditional context return
-- fault whether or not the comparison evaluates true, because the fault is a property of the
-- instruction encoding
theorem conditional_context_return_unconditional (q : State) (csr : CsrReg) (cmp : CmpFlag)
    (hu : q.userMode = false) (hc : Ne cmp CmpFlag.always)
    (h : csr = CsrReg.retsc \/ csr = CsrReg.retir) :
    executeJmpCsr q csr cmp = q.fault (CsrCause.conditionalContextReturn cmp csr) := by
  cases h with
  | inl he =>
    subst he
    simp [executeJmpCsr, hu, hc]
  | inr he =>
    subst he
    simp [executeJmpCsr, hu, hc]


-- A HALT in User Mode raises a privileged instruction fault
theorem halt_privileged_in_user_mode (q : State) (rs : GpReg) (hu : q.userMode = true) :
    executeHalt q rs = q.fault CsrCause.privilegedInstruction := by
  simp [executeHalt, hu]


-- A SYSCALL in User Mode is not privileged, since it is how User Mode reaches Machine Mode
theorem syscall_allowed_in_user_mode (q : State) (rs : GpReg) (_hu : q.userMode = true)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) (hpc : q.pc < 0xFFFE#16)
    (hv : q.syscallVectorReadable) :
    Ne (executeSyscall q rs) (q.fault CsrCause.privilegedInstruction) := by
  intro he
  have hr : q.readVector 0xFFFA = Except.ok q.syscallVector := hv
  have h1 : (executeSyscall q rs).inSyscall = true := by
    simp only [executeSyscall, hr]
    simp [hs, hi, linkAddr_of_lt q.pc hpc]
  rw [he, (fault_preserves_context q CsrCause.privilegedInstruction).1, hs] at h1
  simp at h1


-- No WSR can reach the internal slots holding the automatically saved RSCRATCH, FLAGS, and PRIVS
-- values, which have no special register encoding and so cannot be read either
theorem wsr_cannot_reach_int_slots (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg) :
    let q' := executeWsr q rs1 rs2 csr
    q'.intRscratch = q.intRscratch
      /\ q'.intFlags = q.intFlags
      /\ q'.intPrivs = q.intPrivs
      /\ q'.syscallPrivs = q.syscallPrivs := by
  simp only [executeWsr]
  split
  next => exact (State.fault_int_frame _ _).2
  next => simp [State.next_int_frame, State.writeSpecial_int_frame]


-- Executing a decoded instruction leaves the pending interrupt flag and the interrupt save slots
-- unchanged, because no instruction names them and only taking an interrupt writes them
theorem decodeAndExecute_int_frame (q : State) :
    (decodeAndExecute q).intPending = q.intPending
      /\ (decodeAndExecute q).intRscratch = q.intRscratch
      /\ (decodeAndExecute q).intFlags = q.intFlags
      /\ (decodeAndExecute q).intPrivs = q.intPrivs := by
  simp only [decodeAndExecute, executeHalt, executeSyscall, executeAdd, executeAdc, executeSub,
             executeSbb, executeAnd, executeOr, executeXor, executeRrc, executeSet, executeLd,
             executeSt, executeRsr, executeWsr, executeJal, executeJmp, executeJmpCsr]
  repeat (any_goals split)
  all_goals
    simp [State.writeReg_int_frame, State.writeSpecial_int_frame, State.next_int_frame,
          State.writeMem, State.fault_int_frame, State.faultAt_int_frame, State.writeFlags,
          State.setCsr, State.setMem]


-- Fetching and executing leaves the pending interrupt flag and the interrupt save slots unchanged
theorem execute_int_frame (q : State) :
    (execute q).intPending = q.intPending
      /\ (execute q).intRscratch = q.intRscratch
      /\ (execute q).intFlags = q.intFlags
      /\ (execute q).intPrivs = q.intPrivs := by
  simp only [execute]
  split
  next => simp [State.fault_int_frame]
  next => exact decodeAndExecute_int_frame q


-- Only taking an interrupt writes the interrupt save slots, so an interrupt handler cannot forge
-- the context it will return to
theorem step_preserves_int_slots (q : State)
    (h : Not ((q.intPending && q.canInterrupt) = true)) :
    let q' := step q
    q'.intRscratch = q.intRscratch /\ q'.intFlags = q.intFlags /\ q'.intPrivs = q.intPrivs := by
  simp only [step]
  split
  next => exact And.intro rfl (And.intro rfl rfl)
  next =>
    have hf := execute_int_frame { q with intPending := false }
    exact And.intro hf.2.1 (And.intro hf.2.2.1 hf.2.2.2)


-- Taking an interrupt leaves the system call PRIVS slot alone, so an interrupt inside a system call
-- context does not disturb the value the system call return will restore
theorem interrupt_preserves_syscall_privs (q : State) :
    q.interrupt.syscallPrivs = q.syscallPrivs := by
  simp only [State.interrupt]
  split
  next => rfl
  next => exact (State.faultAt_int_frame _ _ _).2.2.2.2


-- A SYSCALL saves the current PRIVS value into the system call slot
theorem syscall_saves_privs (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) (hpc : q.pc < 0xFFFE#16)
    (hv : q.syscallVectorReadable) :
    (executeSyscall q rs).syscallPrivs = q.special CsrReg.privs := by
  have hr : q.readVector 0xFFFA = Except.ok q.syscallVector := hv
  simp only [executeSyscall, hr]
  simp [hs, hi, linkAddr_of_lt q.pc hpc]


-- Taking an interrupt saves the current PRIVS value into the interrupt slot
theorem interrupt_saves_privs (q : State) (hv : q.intVectorReadable) :
    q.interrupt.intPrivs = q.special CsrReg.privs := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  simp only [State.interrupt, h, State.intVector, State.memory]


-- Taking an interrupt sets RETIR to the address of the preempted instruction, so the return
-- resumes with that instruction rather than skipping it
theorem interrupt_saves_preempted_pc (q : State) (hv : q.intVectorReadable) :
    q.interrupt.special CsrReg.retir = q.pc := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  simp only [State.interrupt, h, State.intVector, State.memory]
  simp [State.writeSpecial, CsrReg.enc, CsrReg.writeMask, State.special, State.setCsr] <;> bv_decide


-- A SYSCALL sets RETSC to the address after itself, since the SYSCALL is the instruction that
-- entered the context
theorem syscall_saves_next_pc (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) (hpc : q.pc < 0xFFFE#16)
    (hv : q.syscallVectorReadable) :
    (executeSyscall q rs).special CsrReg.retsc = q.pc + 2 := by
  have hr : q.readVector 0xFFFA = Except.ok q.syscallVector := hv
  simp only [executeSyscall, hr]
  simp [hs, hi, linkAddr_of_lt q.pc hpc, State.writeSpecial, CsrReg.enc,
        CsrReg.writeMask, State.special, State.setCsr] <;> bv_decide


-- Taking an interrupt continues execution at the interrupt handler address in the reset vector
-- table
theorem interrupt_jumps_to_vector (q : State) (hv : q.intVectorReadable) :
    q.interrupt.pc = q.intVector := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  simp only [State.interrupt, h, State.intVector, State.memory]


-- A SYSCALL continues execution at the syscall handler address in the reset vector table
theorem syscall_jumps_to_vector (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) (hpc : q.pc < 0xFFFE#16)
    (hv : q.syscallVectorReadable) :
    (executeSyscall q rs).pc = q.syscallVector := by
  have hr : q.readVector 0xFFFA = Except.ok q.syscallVector := hv
  simp only [executeSyscall, hr]
  simp [hs, hi, linkAddr_of_lt q.pc hpc, State.syscallVector]


-- Taking an interrupt leaves CAUSE unchanged, unlike a system call or a fault
theorem interrupt_preserves_cause (q : State) (hv : q.intVectorReadable) :
    q.interrupt.special CsrReg.cause = q.special CsrReg.cause := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  simp only [State.interrupt, h, State.intVector, State.memory]
  simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]


-- A JMP to RETIR from an interrupt context restores the saved registers, flags, and PRIVS from the
-- internal slots, branches to the saved program counter, and leaves the context. The ring bit goes
-- through the automatic update, which is a no-op while the Privilege Extension is disabled
theorem retir_restores (q : State) (hu : q.userMode = false) (hi : q.inInterrupt = true) :
    let q' := executeJmpCsr q CsrReg.retir CmpFlag.always
    q'.register GpReg.rscratch0 = q.intRscratch.extractLsb' 8 8
      /\ q'.register GpReg.rscratch1 = q.intRscratch.extractLsb' 0 8
      /\ q'.special CsrReg.flags = (CsrFlags.dec q.intFlags).enc
      /\ q'.special CsrReg.privs = q.privsUpdate q.intPrivs &&& CsrReg.privs.writeMask
      /\ q'.pc = q.special CsrReg.retir
      /\ q'.inInterrupt = false := by
  simp only [executeJmpCsr]
  split
  next hg =>
    rw [hu] at hg
    simp at hg
  next =>
    split
    next hc => simp at hc
    next =>
      refine And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_ rfl))))
      all_goals
        simp [State.register, State.special, State.writeSpecial, State.writeFlags, State.writeReg,
              State.setCsr, State.setReg, GpReg.enc, CsrReg.enc, CsrReg.writeMask]


-- Under the Privilege Extension a JMP to RETIR restores exactly the ring bit the interrupt
-- displaced
theorem retir_restores_privs (q : State) (hu : q.userMode = false) (hi : q.inInterrupt = true)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = true) :
    (executeJmpCsr q CsrReg.retir CmpFlag.always).special CsrReg.privs
      = q.intPrivs &&& CsrReg.privs.writeMask := by
  rw [(retir_restores q hu hi).2.2.2.1, privsUpdate_enabled q q.intPrivs h]


-- A JMP to RETIR leaves PRIVS alone while the Privilege Extension is disabled, so the saved ring
-- bit stays in its slot instead of reappearing in a register the extension no longer governs
theorem retir_preserves_privs_when_extension_disabled (q : State) (hwf : q.WF)
    (hu : q.userMode = false) (hi : q.inInterrupt = true)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    (executeJmpCsr q CsrReg.retir CmpFlag.always).special CsrReg.privs
      = q.special CsrReg.privs := by
  rw [(retir_restores q hu hi).2.2.2.1, privsUpdate_disabled q q.intPrivs h]
  exact privs_mask_self q hwf


-- Under the Privilege Extension a JMP to RETIR from an interrupt context leaves PRIVS holding the
-- saved ring bit, or a cleared one if the return was itself privileged because the context had
-- raised the level to User Mode
theorem retir_discards_privs (q : State) (hi : q.inInterrupt = true)
    (he : (CsrExtns.dec (q.special CsrReg.extns)).privilege = true) :
    (executeJmpCsr q CsrReg.retir CmpFlag.always).special CsrReg.privs
        = q.intPrivs &&& CsrReg.privs.writeMask
      \/ (executeJmpCsr q CsrReg.retir CmpFlag.always).special CsrReg.privs = 0x0000 := by
  cases hum : q.userMode
  next => exact Or.inl (retir_restores_privs q hum hi he)
  next =>
    refine Or.inr ?_
    rw [jmpCsr_privilege_precedence_retir q CmpFlag.always hum, fault_privs_update,
        privsUpdate_enabled q 0x0000 he]
    bv_decide


-- A WSR to PRIVS inside an interrupt context does not survive the return. A cleared ring bit
-- leaves the return to restore the saved one, and a set ring bit enters User Mode, where the
-- return is a privileged instruction and the fault it raises clears the bit again.
theorem wsr_privs_does_not_survive_return (q : State) (_hwf : q.WF) (rs1 : GpReg) (rs2 : GpReg)
    (hu : q.userMode = false) (hi : q.inInterrupt = true) (hpc : q.pc < 0xFFF0#16)
    (he : (CsrExtns.dec (q.special CsrReg.extns)).privilege = true) :
    let q1 := executeWsr q rs1 rs2 CsrReg.privs
    let q2 := executeJmpCsr q1 CsrReg.retir CmpFlag.always
    q2.special CsrReg.privs = q.intPrivs &&& CsrReg.privs.writeMask
      \/ q2.special CsrReg.privs = 0x0000 := by
  -- the WSR commits its value and advances, since the program counter is far from the end of the
  -- address space
  have hlt : (q.writeSpecial CsrReg.privs (q.register rs1 ++ q.register rs2)).pc < 0xFFFE#16 := by
    rw [State.writeSpecial_pc]
    bv_decide
  have hw : executeWsr q rs1 rs2 CsrReg.privs
      = State.next (q.writeSpecial CsrReg.privs (q.register rs1 ++ q.register rs2)) := by
    simp [executeWsr, hu]
  simp only [hw]
  -- the WSR reaches neither EXTNS nor the interrupt context nor the saved ring bit, so the return
  -- it runs into is the one the interrupt set up
  have hex : (State.next (q.writeSpecial CsrReg.privs (q.register rs1 ++ q.register rs2))).special
      CsrReg.extns = q.special CsrReg.extns := by
    rw [next_special_of_lt _ CsrReg.extns hlt,
        State.writeSpecial_other q CsrReg.privs CsrReg.extns _ (by decide)]
  have hin : (State.next (q.writeSpecial CsrReg.privs
      (q.register rs1 ++ q.register rs2))).inInterrupt = true := by
    simp only [State.next]
    split
    next => rw [(fault_preserves_context _ CsrCause.pcOverflow).2]; exact hi
    next => exact hi
  have hip : (State.next (q.writeSpecial CsrReg.privs
      (q.register rs1 ++ q.register rs2))).intPrivs = q.intPrivs := by
    rw [(State.next_int_frame _).2.2.2.1, (State.writeSpecial_int_frame q CsrReg.privs _).2.2.2.1]
  have hor := retir_discards_privs _ hin (by rw [hex]; exact he)
  rw [hip] at hor
  exact hor


-- An interrupt and its immediate return are a round trip on the ring bit either way the Privilege
-- Extension is set: the return replays the saved bit while the extension is enabled, and neither
-- the entry nor the return touches PRIVS while it is disabled
theorem interrupt_privs_round_trip (q : State) (hwf : q.WF) (hv : q.intVectorReadable) :
    q.interrupt.privsUpdate q.interrupt.intPrivs &&& CsrReg.privs.writeMask
      = q.special CsrReg.privs := by
  have he : q.interrupt.special CsrReg.extns = q.special CsrReg.extns := by
    simp only [State.interrupt]
    split
    next => simp [State.writeSpecial, CsrReg.enc, State.special, State.setCsr]
    next => exact (faultAt_frame _ _ _).2.2.2.2.2.2.1
  rw [interrupt_saves_privs q hv]
  unfold State.privsUpdate
  rw [he]
  split
  next => exact privs_mask_self q hwf
  next hc =>
    have hd : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false := by simpa using hc
    rw [interrupt_preserves_privs_when_extension_disabled q hwf hd]
    exact privs_mask_self q hwf


-- An interrupt that returns immediately restores RSCRATCH0, RSCRATCH1, FLAGS, PRIVS, and the
-- program counter, so an empty interrupt handler is transparent
theorem interrupt_transparent (q : State) (hwf : q.WF) (hv : q.intVectorReadable) :
    let q' := executeJmpCsr q.interrupt CsrReg.retir CmpFlag.always
    q'.register GpReg.rscratch0 = q.register GpReg.rscratch0
      /\ q'.register GpReg.rscratch1 = q.register GpReg.rscratch1
      /\ q'.special CsrReg.flags = q.special CsrReg.flags
      /\ q'.special CsrReg.privs = q.special CsrReg.privs
      /\ q'.pc = q.pc
      /\ q'.inInterrupt = false := by
  have h : q.readVector 0xFFF8 = Except.ok q.intVector := hv
  have hin : q.interrupt.inInterrupt = true := by simp only [State.interrupt, h]
  have hr := retir_restores q.interrupt (interrupt_enters_machine_mode q) hin
  have hir : q.interrupt.intRscratch
      = q.register GpReg.rscratch0 ++ q.register GpReg.rscratch1 := by
    simp only [State.interrupt, h]
  have hif : q.interrupt.intFlags = q.special CsrReg.flags := by simp only [State.interrupt, h]
  have hfl : (CsrFlags.dec (q.special CsrReg.flags)).enc = q.special CsrReg.flags :=
    CsrFlags.enc_dec (q.special CsrReg.flags) (hwf.csrMasked CsrReg.flags)
  refine And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_
    (And.intro ?_ hr.2.2.2.2.2))))
  next =>
    rw [hr.1, hir]
    bv_decide
  next =>
    rw [hr.2.1, hir]
    bv_decide
  next => rw [hr.2.2.1, hif, hfl]
  next =>
    rw [hr.2.2.2.1]
    exact interrupt_privs_round_trip q hwf hv
  next =>
    rw [hr.2.2.2.2.1]
    exact interrupt_saves_preempted_pc q hv


-- A SYSCALL in a system call or interrupt context raises an improper system call fault
theorem no_nested_syscall (q : State) (rs : GpReg)
    (h : q.inSyscall = true \/ q.inInterrupt = true) :
    executeSyscall q rs = q.fault (CsrCause.improperSystemCall q.inSyscall q.inInterrupt) := by
  cases h with
  | inl he => simp [executeSyscall, he]
  | inr he => simp [executeSyscall, he]


-- An interrupt cannot be taken from an interrupt context
theorem no_nested_interrupt (q : State) (h : q.inInterrupt = true) : q.canInterrupt = false := by
  simp [State.canInterrupt, h]


-- A JMP to RETSC outside a system call context, or inside an interrupt context, raises an improper
-- return context fault
theorem retsc_improper_context (q : State) (hu : q.userMode = false)
    (h : q.inSyscall = false \/ q.inInterrupt = true) :
    executeJmpCsr q CsrReg.retsc CmpFlag.always
      = q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt CsrReg.retsc) := by
  cases h with
  | inl he => simp [executeJmpCsr, hu, he]
  | inr he => simp [executeJmpCsr, hu, he]


-- A JMP to RETIR outside an interrupt context raises an improper return context fault
theorem retir_improper_context (q : State) (hu : q.userMode = false) (h : q.inInterrupt = false) :
    executeJmpCsr q CsrReg.retir CmpFlag.always
      = q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt CsrReg.retir) := by
  simp [executeJmpCsr, hu, h]


-- A JMP to RETSC from a system call context leaves the context, branches to RETSC, and restores
-- the saved PRIVS value through the automatic update, which is a no-op while the Privilege
-- Extension is disabled
theorem retsc_leaves_syscall_context (q : State) (_hwf : q.WF) (hu : q.userMode = false)
    (hs : q.inSyscall = true) (hi : q.inInterrupt = false) :
    let q' := executeJmpCsr q CsrReg.retsc CmpFlag.always
    q'.inSyscall = false
      /\ q'.pc = q.special CsrReg.retsc
      /\ q'.special CsrReg.privs = q.privsUpdate q.syscallPrivs &&& CsrReg.privs.writeMask := by
  simp [executeJmpCsr, hu, hs, hi, State.writeSpecial, State.special, State.setCsr]


-- Under the Privilege Extension a JMP to RETSC restores exactly the ring bit the SYSCALL displaced
theorem retsc_restores_privs (q : State) (hwf : q.WF) (hu : q.userMode = false)
    (hs : q.inSyscall = true) (hi : q.inInterrupt = false)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = true) :
    (executeJmpCsr q CsrReg.retsc CmpFlag.always).special CsrReg.privs
      = q.syscallPrivs &&& CsrReg.privs.writeMask := by
  rw [(retsc_leaves_syscall_context q hwf hu hs hi).2.2, privsUpdate_enabled q q.syscallPrivs h]


-- A JMP to RETSC leaves PRIVS alone while the Privilege Extension is disabled, so a handler which
-- disables the extension cannot return a set ring bit into a register the extension no longer
-- governs and have a later fault lose it
theorem retsc_preserves_privs_when_extension_disabled (q : State) (hwf : q.WF)
    (hu : q.userMode = false) (hs : q.inSyscall = true) (hi : q.inInterrupt = false)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) :
    (executeJmpCsr q CsrReg.retsc CmpFlag.always).special CsrReg.privs
      = q.special CsrReg.privs := by
  rw [(retsc_leaves_syscall_context q hwf hu hs hi).2.2, privsUpdate_disabled q q.syscallPrivs h]
  exact privs_mask_self q hwf


-- Taking an interrupt does not change whether the processor is in a system call context
theorem interrupt_preserves_syscall_context (q : State) :
    q.interrupt.inSyscall = q.inSyscall := by
  simp only [State.interrupt]
  split
  next => rfl
  next => exact (faultAt_preserves_context _ _ _).1


-- Stepping a halted processor leaves it unchanged, so HALT is terminal and a halted processor does
-- not take interrupts
theorem halted_is_fixpoint (q : State) (h : q.halted = true) : step q = q := by
  simp [step, h]


-- A HALT halts the processor and records the halt cause with the RS register as extended status
theorem halt_sets_halted (q : State) (rs : GpReg) (hu : q.userMode = false) :
    (executeHalt q rs).halted = true
      /\ (executeHalt q rs).special CsrReg.cause
           = (CsrCause.haltInstruction (q.register rs)).enc &&& CsrReg.cause.writeMask := by
  refine And.intro ?_ ?_
  next => simp [executeHalt, hu]
  next => simp [executeHalt, hu, State.writeSpecial, State.special, State.setCsr]


-- Every step clears the pending interrupt flag, so pin activity that cannot be taken is discarded
-- rather than retained and the source has to reassert the pin
theorem step_clears_int_pending (q : State) (h : q.halted = false) :
    (step q).intPending = false := by
  simp only [step, h]
  split
  next hc => simp at hc
  next =>
    split
    next =>
      simp only [State.interrupt]
      split
      next => rfl
      next => exact (State.faultAt_int_frame _ _ _).1
    next => rw [(execute_int_frame _).1]


-- A pending interrupt is taken before the next instruction is fetched or executed, so an
-- instruction that would have faulted raises its fault only after the interrupt returns
theorem interrupt_preempts_instruction (q : State) (h : q.halted = false)
    (hp : q.intPending = true) (hc : q.canInterrupt = true) :
    step q = q.interrupt := by
  simp [step, h, hp, hc]


end MisaSpec

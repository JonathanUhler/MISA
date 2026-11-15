import MisaProof.Encoding


namespace MisaSpec


-- A state in which R0 reads as zero, every reserved special register bit reads as zero, and the
-- cached check function agrees with the FLAGS special register
structure State.WF (q : State) : Prop where
  zeroReg   : q.register GpReg.r0 = 0x00#8
  csrMasked : forall c : CsrReg, q.special c &&& ~~~ c.writeMask = 0x0000#16
  checkSync : forall f : CmpFlag,
    q.check f = CsrFlags.eval (CsrFlags.dec (q.special CsrReg.flags)) f


-- In a well-formed state the ALWAYS comparison is taken
theorem State.WF.check_always (q : State) (h : q.WF) : q.check CmpFlag.always = true := by
  simp [h.checkSync CmpFlag.always, CsrFlags.eval]


-- A register write to R0 leaves the state unchanged
theorem State.writeReg_r0 (q : State) (v : DataWord) : q.writeReg GpReg.r0 v = q := rfl


-- A register write to a register other than R0 is readable back from that register
theorem State.writeReg_self (q : State) (r : GpReg) (v : DataWord) (h : Ne r GpReg.r0) :
    (q.writeReg r v).register r = v := by
  cases r
  case r0 => exact absurd rfl h
  all_goals simp [State.writeReg]


-- A register write leaves every other register unchanged
theorem State.writeReg_other (q : State) (r : GpReg) (r' : GpReg) (v : DataWord) (h : Ne r' r) :
    (q.writeReg r v).register r' = q.register r' := by
  have he : Ne r'.enc r.enc := fun hEq => h (GpReg.enc_inj r' r hEq)
  cases r
  case r0 => rfl
  all_goals simp [State.writeReg, he]


-- A special register write other than to EXTNS is readable back masked by its write mask
theorem State.writeSpecial_self (q : State) (c : CsrReg) (v : SpecWord)
    (hd : Ne c CsrReg.extns) :
    (q.writeSpecial c v).special c = v &&& c.writeMask := by
  cases c
  case extns => exact absurd rfl hd
  case flags =>
    have hm : (v &&& CsrReg.flags.writeMask) &&& ~~~ CsrReg.flags.writeMask = 0x0000#16 := by
      simp only [CsrReg.writeMask]
      bv_decide
    simp [State.writeSpecial, State.writeFlags, CsrFlags.enc_dec _ hm]
  all_goals simp [State.writeSpecial]


-- A special register write leaves every other special register unchanged
theorem State.writeSpecial_other (q : State) (c : CsrReg) (c' : CsrReg) (v : SpecWord)
    (h : Ne c' c) :
    (q.writeSpecial c v).special c' = q.special c' := by
  have he : Ne c'.enc c.enc := fun hEq => h (CsrReg.enc_inj c' c hEq)
  cases c
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals simp [he]
  all_goals simp [State.writeSpecial, State.writeFlags, he]


-- No special register write can set a reserved bit
theorem State.writeSpecial_masked (q : State) (c : CsrReg) (c' : CsrReg) (v : SpecWord)
    (h : q.WF) :
    (q.writeSpecial c v).special c' &&& ~~~ c'.writeMask = 0x0000#16 := by
  by_cases hc : c' = c
  case neg =>
    rw [State.writeSpecial_other q c c' v hc]
    exact h.csrMasked c'
  case pos =>
    rw [hc]
    by_cases he : c = CsrReg.extns
    case pos =>
      rw [he]
      simp only [State.writeSpecial]
      split
      next => simp [CsrReg.writeMask] <;> bv_decide
      next => exact h.csrMasked CsrReg.extns
    case neg =>
      rw [State.writeSpecial_self q c v he]
      cases c <;> (simp only [CsrReg.writeMask]; bv_decide)


-- A flags write updates the cached check function to agree with the new FLAGS value
theorem State.writeFlags_sync (q : State) (f : CsrFlags) (cmp : CmpFlag) :
    (q.writeFlags f).check cmp
      = CsrFlags.eval (CsrFlags.dec ((q.writeFlags f).special CsrReg.flags)) cmp := by
  simp [State.writeFlags, CsrFlags.dec_enc]


-- A special register write naming FLAGS also refreshes the cached check function
theorem State.writeSpecial_flags_sync (q : State) (v : SpecWord) (cmp : CmpFlag) :
    (q.writeSpecial CsrReg.flags v).check cmp
      = CsrFlags.eval (CsrFlags.dec (v &&& CsrReg.flags.writeMask)) cmp := by
  simp [State.writeSpecial, State.writeFlags]


-- A register write leaves every special register unchanged
theorem State.writeReg_special (q : State) (r : GpReg) (v : DataWord) (c : CsrReg) :
    (q.writeReg r v).special c = q.special c := by
  cases r <;> rfl


-- A register write leaves the interrupt save slots, the system call slot, and the pending
-- interrupt flag unchanged
theorem State.writeReg_int_frame (q : State) (r : GpReg) (v : DataWord) :
    (q.writeReg r v).intPending = q.intPending
      /\ (q.writeReg r v).intRscratch = q.intRscratch
      /\ (q.writeReg r v).intFlags = q.intFlags
      /\ (q.writeReg r v).intPrivs = q.intPrivs
      /\ (q.writeReg r v).syscallPrivs = q.syscallPrivs := by
  cases r <;> exact And.intro rfl (And.intro rfl (And.intro rfl (And.intro rfl rfl)))


-- A special register write leaves the interrupt save slots, the system call slot, and the pending
-- interrupt flag unchanged
theorem State.writeSpecial_int_frame (q : State) (c : CsrReg) (v : SpecWord) :
    (q.writeSpecial c v).intPending = q.intPending
      /\ (q.writeSpecial c v).intRscratch = q.intRscratch
      /\ (q.writeSpecial c v).intFlags = q.intFlags
      /\ (q.writeSpecial c v).intPrivs = q.intPrivs
      /\ (q.writeSpecial c v).syscallPrivs = q.syscallPrivs := by
  cases c
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals exact And.intro rfl (And.intro rfl (And.intro rfl (And.intro rfl rfl)))
  all_goals exact And.intro rfl (And.intro rfl (And.intro rfl (And.intro rfl rfl)))


-- Advancing the program counter leaves the interrupt save slots, the system call slot, and the
-- pending interrupt flag unchanged, whether or not it overflows into a fault
theorem State.next_int_frame (q : State) :
    (State.next q).intPending = q.intPending
      /\ (State.next q).intRscratch = q.intRscratch
      /\ (State.next q).intFlags = q.intFlags
      /\ (State.next q).intPrivs = q.intPrivs
      /\ (State.next q).syscallPrivs = q.syscallPrivs := by
  simp only [State.next, State.fault, State.writeSpecial]
  repeat (any_goals split)
  all_goals exact And.intro rfl (And.intro rfl (And.intro rfl (And.intro rfl rfl)))


-- A register write leaves memory unchanged
theorem State.writeReg_memory (q : State) (r : GpReg) (v : DataWord) (a : AddrWord) :
    (q.writeReg r v).memory a = q.memory a := by
  cases r <;> rfl


-- A special register write leaves memory unchanged
theorem State.writeSpecial_memory (q : State) (c : CsrReg) (v : SpecWord) (a : AddrWord) :
    (q.writeSpecial c v).memory a = q.memory a := by
  cases c
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals rfl
  all_goals rfl


-- A memory write leaves every other address unchanged
theorem State.writeMem_other (q : State) (a : AddrWord) (a' : AddrWord) (v : DataWord)
    (h : Ne a' a) : (q.writeMem a v).memory a' = q.memory a' := by
  simp [State.writeMem, h]


-- A special register write leaves the program counter unchanged
theorem State.writeSpecial_pc (q : State) (c : CsrReg) (v : SpecWord) :
    (q.writeSpecial c v).pc = q.pc := by
  cases c
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals rfl
  all_goals rfl


-- A register write leaves every comparison unchanged
theorem State.writeReg_check (q : State) (r : GpReg) (v : DataWord) (cmp : CmpFlag) :
    (q.writeReg r v).check cmp = q.check cmp := by
  cases r <;> rfl


-- A special register write other than to FLAGS leaves every comparison unchanged
theorem State.writeSpecial_check (q : State) (c : CsrReg) (v : SpecWord) (cmp : CmpFlag)
    (h : Ne c CsrReg.flags) :
    (q.writeSpecial c v).check cmp = q.check cmp := by
  cases c
  case flags => exact absurd rfl h
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals rfl
  all_goals rfl


-- A special register write leaves every register unchanged
theorem State.writeSpecial_register (q : State) (c : CsrReg) (v : SpecWord) (r : GpReg) :
    (q.writeSpecial c v).register r = q.register r := by
  cases c
  case extns =>
    simp only [State.writeSpecial]
    split
    all_goals rfl
  all_goals rfl


-- Advancing the program counter leaves every register unchanged, whether or not it overflows into
-- a fault
theorem State.next_register (q : State) (r : GpReg) :
    (State.next q).register r = q.register r := by
  simp only [State.next]
  split
  all_goals rfl


-- Advancing the program counter leaves every comparison unchanged
theorem State.next_check (q : State) (cmp : CmpFlag) :
    (State.next q).check cmp = q.check cmp := by
  simp only [State.next]
  split
  all_goals rfl


-- Advancing the program counter leaves FLAGS unchanged, since the fault it can raise records only
-- CAUSE, PRIVS, and FAULT
theorem State.next_flags (q : State) :
    (State.next q).special CsrReg.flags = q.special CsrReg.flags := by
  simp only [State.next]
  split
  all_goals rfl


-- A register write preserves well-formedness, since a write naming R0 is dropped
theorem State.writeReg_wf (q : State) (r : GpReg) (v : DataWord) (h : q.WF) :
    (q.writeReg r v).WF := by
  cases r
  case r0 => exact h
  all_goals exact
    { zeroReg   := by simp [State.writeReg, GpReg.enc, h.zeroReg],
      csrMasked := h.csrMasked,
      checkSync := h.checkSync }


-- A flags write preserves well-formedness, since it refreshes the cached check function alongside
-- the FLAGS special register
theorem State.writeFlags_wf (q : State) (f : CsrFlags) (h : q.WF) : (q.writeFlags f).WF := by
  constructor
  next => exact h.zeroReg
  next =>
    intro c
    cases c
    case flags => simp [State.writeFlags, CsrFlags.enc, CsrReg.writeMask] <;> bv_decide
    all_goals exact h.csrMasked _
  next => exact State.writeFlags_sync q f


-- A special register write preserves well-formedness
theorem State.writeSpecial_wf (q : State) (c : CsrReg) (v : SpecWord) (h : q.WF) :
    (q.writeSpecial c v).WF := by
  cases c
  case flags => exact State.writeFlags_wf q (CsrFlags.dec (v &&& CsrReg.flags.writeMask)) h
  case extns =>
    constructor
    next =>
      simp only [State.writeSpecial]
      split
      all_goals exact h.zeroReg
    next => exact fun c' => State.writeSpecial_masked q CsrReg.extns c' v h
    next =>
      intro cmp
      simp only [State.writeSpecial]
      split
      all_goals exact h.checkSync cmp
  all_goals
    constructor
    next => exact h.zeroReg
    next => exact fun c' => State.writeSpecial_masked q _ c' v h
    next => exact h.checkSync


-- A memory write preserves well-formedness
theorem State.writeMem_wf (q : State) (a : AddrWord) (v : DataWord) (h : q.WF) :
    (q.writeMem a v).WF :=
  { zeroReg := h.zeroReg, csrMasked := h.csrMasked, checkSync := h.checkSync }


-- Taking a fault preserves well-formedness
theorem State.fault_wf (q : State) (c : CsrCause) (h : q.WF) : (q.fault c).WF := by
  have h1 := State.writeSpecial_wf q CsrReg.cause c.enc h
  have h2 := State.writeSpecial_wf _ CsrReg.privs 0x0000 h1
  have h3 := State.writeSpecial_wf _ CsrReg.fault q.pc h2
  exact { zeroReg := h3.zeroReg, csrMasked := h3.csrMasked, checkSync := h3.checkSync }


-- Advancing the program counter preserves well-formedness
theorem State.next_wf (q : State) (h : q.WF) : (State.next q).WF := by
  simp only [State.next]
  split
  next => exact State.fault_wf q CsrCause.pcOverflow h
  next => exact { zeroReg := h.zeroReg, csrMasked := h.csrMasked, checkSync := h.checkSync }


-- Taking an interrupt preserves well-formedness
theorem State.interrupt_wf (q : State) (h : q.WF) : q.interrupt.WF := by
  have h1 := State.writeSpecial_wf q CsrReg.retir q.pc h
  have h2 := State.writeSpecial_wf _ CsrReg.privs 0x0000 h1
  exact { zeroReg := h2.zeroReg, csrMasked := h2.csrMasked, checkSync := h2.checkSync }


-- Reset produces a well-formed state
theorem State.first_wf (bytes : ByteArray) (extns : SpecWord) : (State.first bytes extns).WF := by
  constructor
  next => rfl
  next =>
    intro c
    cases c <;> simp [State.first, CsrReg.enc, CsrReg.writeMask] <;> bv_decide
  next =>
    intro f
    rfl


-- Reset initializes the program counter from the reset vector
theorem State.first_pc (bytes : ByteArray) (extns : SpecWord) :
    (State.first bytes extns).pc = (State.first bytes extns).resetVector :=
  rfl


-- Reset starts outside any system call or interrupt context and discards pre-reset interrupt pin
-- activity
theorem State.first_contexts (bytes : ByteArray) (extns : SpecWord) :
    (State.first bytes extns).inSyscall = false
      /\ (State.first bytes extns).inInterrupt = false
      /\ (State.first bytes extns).intPending = false :=
  And.intro rfl (And.intro rfl rfl)


-- Deserializing a state file produces a well-formed state, so a hand-written or corrupted file
-- cannot put the model somewhere step was never proved correct on
theorem State.deserialize_wf (content : String) (q : State)
    (h : State.deserialize content = some q) :
    q.WF := by
  cases hh : (lookup? (parseStateText content).scalars "halted").bind parseBool? with
  | none => simp [State.deserialize, hh] at h
  | some halted =>
    cases hp : (lookup? (parseStateText content).scalars "pc").bind parseNumeral? with
    | none => simp [State.deserialize, hh, hp] at h
    | some pcN =>
      simp [State.deserialize, hh, hp] at h
      subst h
      constructor
      next => rfl
      next =>
        intro c
        have hm : forall x : SpecWord, (x &&& c.writeMask) &&& ~~~ c.writeMask = 0x0000#16 := by
          intro x
          bv_decide
        cases hc : (lookup? (parseStateText content).specials c.name).bind parseNumeral? with
        | none   => simp [hc] <;> bv_decide
        | some n => simp [hc, hm] <;> bv_decide
      next =>
        intro f
        rfl


-- Well-formedness constrains only the registers, the special registers, and the cached
-- comparisons, so an update leaving those three alone preserves it
theorem State.wf_of_fields (q : State) (q' : State) (h : q.WF)
    (hr : q'.register = q.register) (hs : q'.special = q.special) (hc : q'.check = q.check) :
    q'.WF := by
  constructor
  next => rw [hr]; exact h.zeroReg
  next => rw [hs]; exact h.csrMasked
  next => rw [hc, hs]; exact h.checkSync


-- Executing a decoded instruction preserves well-formedness, because every instruction reaches
-- its next state through some composition of a register write, a special register write, a memory
-- write, a flags write, a fault, and the program counter increment
theorem State.decodeAndExecute_wf (q : State) (h : q.WF) : (decodeAndExecute q).WF := by
  simp only [decodeAndExecute, executeHalt, executeSyscall, executeAdd, executeAdc, executeSub,
             executeSbb, executeAnd, executeOr, executeXor, executeRrc, executeSet, executeLd,
             executeSt, executeRsr, executeWsr, executeJal, executeJmp, executeJmpCsr]
  repeat (any_goals split)
  all_goals
    first
      | exact h
      | exact State.fault_wf _ _ h
      | exact State.next_wf _ h
      | exact State.next_wf _ (State.writeReg_wf _ _ _ h)
      | exact State.next_wf _ (State.writeMem_wf _ _ _ h)
      | exact State.next_wf _ (State.writeSpecial_wf _ _ _ h)
      | exact State.next_wf _ (State.writeFlags_wf _ _ (State.writeReg_wf _ _ _ h))
      | exact State.next_wf _ (State.writeReg_wf _ _ _ (State.writeReg_wf _ _ _ h))
      | exact State.next_wf _ (State.writeReg_wf _ _ _ (State.writeSpecial_wf _ _ _ h))
      | exact State.next_wf _ (State.writeSpecial_wf _ _ _ (State.writeMem_wf _ _ _ h))
      | exact State.wf_of_fields _ _ h rfl rfl rfl
      | exact State.wf_of_fields _ _ (State.writeSpecial_wf _ _ _ h) rfl rfl rfl
      | exact State.wf_of_fields _ _
          (State.writeSpecial_wf _ _ _
            (State.writeSpecial_wf _ _ _ (State.writeSpecial_wf _ _ _ h)))
          rfl rfl rfl
      | exact State.wf_of_fields _ _
          (State.writeSpecial_wf _ _ _
            (State.writeFlags_wf _ _ (State.writeReg_wf _ _ _ (State.writeReg_wf _ _ _ h))))
          rfl rfl rfl


-- Fetching and executing preserves well-formedness
theorem State.execute_wf (q : State) (h : q.WF) : (execute q).WF := by
  simp only [execute]
  split
  next => exact State.fault_wf _ _ h
  next => exact State.decodeAndExecute_wf q h


-- Stepping a well-formed state produces a well-formed state
theorem State.step_wf (q : State) (h : q.WF) : (step q).WF := by
  simp only [step]
  split
  next => exact h
  next =>
    split
    next => exact State.interrupt_wf q h
    next => exact State.execute_wf _ (State.wf_of_fields _ _ h rfl rfl rfl)


end MisaSpec

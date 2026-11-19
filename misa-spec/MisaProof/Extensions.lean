import MisaProof.Wf


namespace MisaSpec


-- A write to EXTNS leaves bit 0 unchanged, so the Dynamic Hardware Extension cannot disable itself
theorem extns_bit0_ignores_writes (q : State) (v : SpecWord) :
    ((q.writeSpecial CsrReg.extns v).special CsrReg.extns).getLsbD 0
      = (q.special CsrReg.extns).getLsbD 0 := by
  simp only [State.writeSpecial]
  split
  next => simp [CsrReg.writeMask] <;> bv_decide
  next => rfl


-- Without the Dynamic Hardware Extension a write to EXTNS is ignored entirely, so the supported
-- extension set is a static property of the hardware
theorem extns_static_without_dynamichw (q : State) (v : SpecWord)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).dynamichw = false) :
    q.writeSpecial CsrReg.extns v = q := by
  simp [State.writeSpecial, h]


-- With the Dynamic Hardware Extension a write to EXTNS takes the written value for the other three
-- extension bits, in time for the next instruction to decode against it
theorem extns_dynamic_write (q : State) (v : SpecWord)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).dynamichw = true) :
    let e := CsrExtns.dec ((q.writeSpecial CsrReg.extns v).special CsrReg.extns)
    e.syscall = v.getLsbD 1 /\ e.privilege = v.getLsbD 2 /\ e.interrupt = v.getLsbD 3 := by
  simp only [State.writeSpecial, h]
  simp [CsrExtns.dec, CsrReg.writeMask] <;> bv_decide


-- A write to EXTNS leaves every other special register unchanged, so disabling an extension does
-- not clear the registers it defines
theorem extns_write_preserves_other_csrs (q : State) (v : SpecWord) (c : CsrReg)
    (h : Ne c CsrReg.extns) :
    (q.writeSpecial CsrReg.extns v).special c = q.special c :=
  State.writeSpecial_other q CsrReg.extns c v h


-- A write to EXTNS does not change whether the processor is in a system call or interrupt context
theorem extns_write_preserves_contexts (q : State) (v : SpecWord) :
    (q.writeSpecial CsrReg.extns v).inSyscall = q.inSyscall
      /\ (q.writeSpecial CsrReg.extns v).inInterrupt = q.inInterrupt := by
  simp only [State.writeSpecial]
  split
  all_goals exact And.intro rfl rfl


-- An instruction word that decodes under some extension set decodes to an instruction available
-- under that set, and encodes back to the word
theorem decUnder_available (w : InstWord) (e : CsrExtns) (i : Inst)
    (h : Inst.decUnder w e = some i) :
    i.availableUnder e = true /\ i.enc = w := by
  cases hx : Inst.dec w with
  | none   => simp [Inst.decUnder, hx] at h
  | some j =>
    simp only [Inst.decUnder, hx] at h
    split at h
    next ha =>
      have hji : j = i := Option.some.inj h
      refine And.intro ?_ ?_
      next =>
        rw [<- hji]
        exact ha
      next =>
        rw [<- hji]
        exact Inst.enc_dec w j hx
    next => simp at h


-- Enabling an extension never makes a special register unavailable
theorem csr_availableUnder_monotone (csr : CsrReg) (e1 : CsrExtns) (e2 : CsrExtns)
    (hle : e1.le e2) (h : csr.availableUnder e1 = true) : csr.availableUnder e2 = true := by
  cases csr
  case retsc => exact hle.2.1 h
  case retir => exact hle.2.2.2 h
  case privs => exact hle.2.2.1 h
  case mpucf => exact hle.2.2.1 h
  all_goals rfl


-- Enabling an extension never makes an instruction unavailable
theorem availableUnder_monotone (i : Inst) (e1 : CsrExtns) (e2 : CsrExtns)
    (hle : e1.le e2) (h : i.availableUnder e1 = true) : i.availableUnder e2 = true := by
  cases i
  case syscall rs       => exact hle.2.1 h
  case rsr rs1 rs2 csr  => exact csr_availableUnder_monotone csr e1 e2 hle h
  case wsr rs1 rs2 csr  => exact csr_availableUnder_monotone csr e1 e2 hle h
  case jmpCsr csr cmp   => exact csr_availableUnder_monotone csr e1 e2 hle h
  all_goals rfl


-- Enabling an extension never invalidates an instruction word that already decoded
theorem decUnder_monotone (w : InstWord) (e1 : CsrExtns) (e2 : CsrExtns) (i : Inst)
    (hle : e1.le e2) (h : Inst.decUnder w e1 = some i) :
    Inst.decUnder w e2 = some i := by
  have hd := decUnder_available w e1 i h
  have hdec : Inst.dec w = some i := by
    rw [<- hd.2]
    exact Inst.dec_enc i
  simp [Inst.decUnder, hdec, availableUnder_monotone i e1 e2 hle hd.1]


-- An instruction naming a special register is available exactly when that register is
theorem availableUnder_eq_csrOperand (i : Inst) (e : CsrExtns) (csr : CsrReg)
    (hc : i.csrOperand = some csr) : i.availableUnder e = csr.availableUnder e := by
  cases i
  case rsr rs1 rs2 c =>
    simp only [Inst.csrOperand, Option.some.injEq] at hc
    simp [Inst.availableUnder, Inst.csrOperand, hc]
  case wsr rs1 rs2 c =>
    simp only [Inst.csrOperand, Option.some.injEq] at hc
    simp [Inst.availableUnder, Inst.csrOperand, hc]
  case jmpCsr c cmp =>
    simp only [Inst.csrOperand, Option.some.injEq] at hc
    simp [Inst.availableUnder, Inst.csrOperand, hc]
  all_goals simp [Inst.csrOperand] at hc


-- No instruction word decodes to a SYSCALL on a core-only implementation
theorem core_decode_no_syscall (w : InstWord) (i : Inst)
    (h : Inst.decUnder w (CsrExtns.dec 0x0000#16) = some i) :
    i.isSyscall = false := by
  have hav := (decUnder_available w (CsrExtns.dec 0x0000#16) i h).1
  cases i
  case syscall rs => simp [Inst.availableUnder, CsrExtns.dec] at hav
  all_goals rfl


-- No instruction word decodes to an instruction naming RETSC, RETIR, PRIVS, or MPUCF on a
-- core-only implementation
theorem core_decode_no_extension_csr (w : InstWord) (i : Inst) (csr : CsrReg)
    (hd : Inst.decUnder w (CsrExtns.dec 0x0000#16) = some i)
    (hc : i.csrOperand = some csr) :
    Ne csr CsrReg.retsc /\ Ne csr CsrReg.retir /\ Ne csr CsrReg.privs
      /\ Ne csr CsrReg.mpucf := by
  have hav := (decUnder_available w (CsrExtns.dec 0x0000#16) i hd).1
  have hcsr : csr.availableUnder (CsrExtns.dec 0x0000#16) = true := by
    rw [<- availableUnder_eq_csrOperand i (CsrExtns.dec 0x0000#16) csr hc]
    exact hav
  cases csr
  case retsc => simp [CsrReg.availableUnder, CsrExtns.dec] at hcsr
  case retir => simp [CsrReg.availableUnder, CsrExtns.dec] at hcsr
  case privs => simp [CsrReg.availableUnder, CsrExtns.dec] at hcsr
  case mpucf => simp [CsrReg.availableUnder, CsrExtns.dec] at hcsr
  all_goals decide


-- Only RSR, WSR, and the special register variant of JMP name a special register as an operand, so
-- the extension gate applies to no other operand position
theorem csrOperand_table (i : Inst) :
    (i.csrOperand.isSome = true)
      <-> (Exists fun rs1 : GpReg => Exists fun rs2 : GpReg => Exists fun csr : CsrReg =>
             i = Inst.rsr rs1 rs2 csr \/ i = Inst.wsr rs1 rs2 csr)
          \/ (Exists fun csr : CsrReg => Exists fun cmp : CmpFlag => i = Inst.jmpCsr csr cmp) := by
  constructor
  next =>
    intro h
    cases i
    case rsr rs1 rs2 csr =>
      exact Or.inl (Exists.intro rs1 (Exists.intro rs2 (Exists.intro csr (Or.inl rfl))))
    case wsr rs1 rs2 csr =>
      exact Or.inl (Exists.intro rs1 (Exists.intro rs2 (Exists.intro csr (Or.inr rfl))))
    case jmpCsr csr cmp =>
      exact Or.inr (Exists.intro csr (Exists.intro cmp rfl))
    all_goals simp [Inst.csrOperand] at h
  next =>
    intro h
    cases i
    case rsr rs1 rs2 csr => rfl
    case wsr rs1 rs2 csr => rfl
    case jmpCsr csr cmp  => rfl
    all_goals simp at h


-- An instruction naming an unavailable special register raises an illegal instruction fault rather
-- than a privilege fault, because the register is absent rather than protected
theorem unavailable_csr_is_illegal (q : State) (w : InstWord) (i : Inst) (csr : CsrReg)
    (hf : fetch q = Except.ok w)
    (hd : Inst.dec w = some i) (hc : i.csrOperand = some csr)
    (ha : csr.availableUnder (CsrExtns.dec (q.special CsrReg.extns)) = false) :
    decodeAndExecute q = q.fault CsrCause.illegalInstruction := by
  have hav : i.availableUnder (CsrExtns.dec (q.special CsrReg.extns)) = false := by
    rw [availableUnder_eq_csrOperand i (CsrExtns.dec (q.special CsrReg.extns)) csr hc]
    exact ha
  have hdu : Inst.decUnder w (CsrExtns.dec (q.special CsrReg.extns)) = none := by
    simp [Inst.decUnder, hd, hav]
  simp [decodeAndExecute, hf, hdu]


-- A SYSCALL encoding raises an illegal instruction fault without the System Call Extension, which
-- is also what the core architecture requires of its reserved exception bit
theorem core_syscall_word_is_illegal (q : State) (rs : GpReg)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).syscall = false)
    (hf : fetch q = Except.ok (Inst.syscall rs).enc) :
    decodeAndExecute q = q.fault CsrCause.illegalInstruction := by
  have hdec : Inst.dec (Inst.syscall rs).enc = some (Inst.syscall rs) :=
    Inst.dec_enc (Inst.syscall rs)
  have hdu :
      Inst.decUnder (Inst.syscall rs).enc (CsrExtns.dec (q.special CsrReg.extns)) = none := by
    simp [Inst.decUnder, hdec, Inst.availableUnder, h]
  simp [decodeAndExecute, hf, hdu]


-- RETSC and RETIR are available under the System Call and Interrupt Extensions respectively, PRIVS
-- and MPUCF are available under the Privilege Extension, and every other special register is
-- always available
theorem availableUnder_table (csr : CsrReg) (e : CsrExtns) :
    csr.availableUnder e
      = match csr with
        | .retsc => e.syscall
        | .retir => e.interrupt
        | .privs => e.privilege
        | .mpucf => e.privilege
        | _      => true := by
  cases csr <;> rfl


-- Every special register other than RETSC, RETIR, PRIVS, and MPUCF is available under any
-- extension set
theorem core_csrs_always_available (csr : CsrReg) (e : CsrExtns)
    (h : Ne csr CsrReg.retsc /\ Ne csr CsrReg.retir /\ Ne csr CsrReg.privs
         /\ Ne csr CsrReg.mpucf) :
    csr.availableUnder e = true := by
  cases csr
  case retsc => exact absurd rfl h.1
  case retir => exact absurd rfl h.2.1
  case privs => exact absurd rfl h.2.2.1
  case mpucf => exact absurd rfl h.2.2.2
  all_goals rfl


end MisaSpec

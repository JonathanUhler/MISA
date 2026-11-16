import MisaProof.Basic


namespace MisaSpec


-- If two GpReg encodings are the same, then the two GpReg constructors are the same
theorem GpReg.enc_inj (r1 : GpReg) (r2 : GpReg) (h : r1.enc = r2.enc) : r1 = r2 := by
  cases r1 <;> cases r2 <;> simp_all [GpReg.enc]


-- If two CsrReg encodings are the same, then the two CsrReg constructors are the same
theorem CsrReg.enc_inj (c1 : CsrReg) (c2 : CsrReg) (h : c1.enc = c2.enc) : c1 = c2 := by
  cases c1 <;> cases c2 <;> simp_all [CsrReg.enc]


-- If two CmpFlag encodings are the same, then the two CmpFlag constructors are the same
theorem CmpFlag.enc_inj (f1 : CmpFlag) (f2 : CmpFlag) (h : f1.enc = f2.enc) : f1 = f2 := by
  cases f1 <;> cases f2 <;> simp_all [CmpFlag.enc]


-- The decode of the encode of a GpReg is that GpReg
theorem GpReg.dec_enc (r : GpReg) : GpReg.dec r.enc = some r := by
  cases r <;> decide


-- The decode of the encode of a CsrReg is that CsrReg
theorem CsrReg.dec_enc (c : CsrReg) : CsrReg.dec c.enc = some c := by
  cases c <;> decide


-- The decode of the encode of a CmpFlag is that CmpFlag
theorem CmpFlag.dec_enc (f : CmpFlag) : CmpFlag.dec f.enc = some f := by
  cases f <;> decide


-- If the decode of a HalfWord is a GpReg, then the encode of the GpReg is the HalfWord
theorem GpReg.enc_dec (n : HalfWord) (r : GpReg) (h : GpReg.dec n = some r) : r.enc = n := by
  have hp := forall_halfWord
    (fun m => match GpReg.dec m with | some r' => r'.enc == m | none => true) (by decide) n
  simpa [h] using hp


-- If the decode of a HalfWord is a CsrReg, then the encode of the CsrReg is the HalfWord
theorem CsrReg.enc_dec (n : HalfWord) (c : CsrReg) (h : CsrReg.dec n = some c) : c.enc = n := by
  have hp := forall_halfWord
    (fun m => match CsrReg.dec m with | some c' => c'.enc == m | none => true) (by decide) n
  simpa [h] using hp


-- If the decode of a CmpWord is a CmpFlag, then the encode of the CmpFlag is the CmpWord
theorem CmpFlag.enc_dec (n : CmpWord) (f : CmpFlag) (h : CmpFlag.dec n = some f) : f.enc = n := by
  have hp := forall_cmpWord
    (fun m => match CmpFlag.dec m with | some f' => f'.enc == m | none => true) (by decide) n
  simpa [h] using hp


-- Every HalfWord has a GpReg decoding
theorem GpReg.dec_isSome (n : HalfWord) : (GpReg.dec n).isSome = true :=
  forall_halfWord (fun m => (GpReg.dec m).isSome) (by decide) n


-- The reserved special register encodings do not decode
theorem CsrReg.dec_eq_none_iff (n : HalfWord) :
    CsrReg.dec n = none
      <-> (n = 0b0000#4 \/ n = 0b0111#4 \/ 0b1011#4 <= n) :=
  of_decide_eq_true (forall_halfWord
    (fun m =>
      decide (CsrReg.dec m = none <-> (m = 0b0000#4 \/ m = 0b0111#4 \/ 0b1011#4 <= m)))
    (by decide) n)


-- The reserved comparison encoding does not decode
theorem CmpFlag.dec_eq_none_iff (n : CmpWord) : CmpFlag.dec n = none <-> n = 0b111#3 :=
  of_decide_eq_true (forall_cmpWord
    (fun m => decide (CmpFlag.dec m = none <-> m = 0b111#3)) (by decide) n)


-- The fields Inst.dec reads out of a word built from four nibbles, which is the shape of every
-- instruction other than HALT, SYSCALL, SET, and the jumps
theorem InstWord.nibbles (a : HalfWord) (b : HalfWord) (c : HalfWord) (d : HalfWord) :
    (a ++ b ++ c ++ d).extractLsb' 0 4 = d
      /\ (a ++ b ++ c ++ d).extractLsb' 4 4 = c
      /\ (a ++ b ++ c ++ d).extractLsb' 8 4 = b
      /\ (a ++ b ++ c ++ d).extractLsb' 12 4 = a := by
  bv_decide


-- The fields Inst.dec reads out of a word built from a byte and two nibbles, which is the shape of
-- HALT, SYSCALL, and SET
theorem InstWord.byteFields (e : DataWord) (f : HalfWord) (g : HalfWord) :
    (e ++ f ++ g).extractLsb' 0 4 = g
      /\ (e ++ f ++ g).extractLsb' 4 4 = f
      /\ (e ++ f ++ g).extractLsb' 8 8 = e
      /\ (e ++ f ++ g).extractLsb' 8 7 = e.extractLsb' 0 7
      /\ (e ++ f ++ g)[15] = e[7] := by
  bv_decide


-- The fields Inst.dec reads out of a word built from a target bit, a comparison, and three
-- nibbles, which is the shape of JAL, JMP, and the special register variant of JMP
theorem InstWord.jumpFields (t : BitVec 1) (cmp : CmpWord) (b : HalfWord) (c : HalfWord)
    (d : HalfWord) :
    (t ++ cmp ++ b ++ c ++ d).extractLsb' 0 4 = d
      /\ (t ++ cmp ++ b ++ c ++ d).extractLsb' 4 4 = c
      /\ (t ++ cmp ++ b ++ c ++ d).extractLsb' 8 4 = b
      /\ (t ++ cmp ++ b ++ c ++ d).extractLsb' 12 3 = cmp
      /\ (t ++ cmp ++ b ++ c ++ d)[15] = t[0] := by
  bv_decide


-- The decode of the encode of an Inst is that Inst
theorem Inst.dec_enc (i : Inst) : Inst.dec i.enc = some i := by
  cases i <;>
    simp [Inst.enc, Inst.dec, InstWord.nibbles, InstWord.byteFields, InstWord.jumpFields,
          GpReg.dec_enc, CsrReg.dec_enc, CmpFlag.dec_enc]


-- The GpReg a HalfWord decodes to, as a total function, since every HalfWord decodes
def GpReg.decD (n : HalfWord) : GpReg := (GpReg.dec n).getD GpReg.r0


-- Decoding a HalfWord as a GpReg always succeeds, which is what collapses the Option binds that
-- Inst.dec performs on a register field
theorem GpReg.dec_eq_decD (n : HalfWord) : GpReg.dec n = some (GpReg.decD n) := by
  have hs : (GpReg.dec n).isSome = true := GpReg.dec_isSome n
  unfold GpReg.decD
  cases hd : GpReg.dec n with
  | none    => simp [hd] at hs
  | some r' => simp


-- The encode of the GpReg a HalfWord decodes to is that HalfWord
theorem GpReg.enc_decD (n : HalfWord) : (GpReg.decD n).enc = n :=
  GpReg.enc_dec n (GpReg.decD n) (GpReg.dec_eq_decD n)


-- The opcode nibble of an instruction word is one of its sixteen values, which is what lets the
-- match in Inst.dec be reduced one arm at a time
theorem InstWord.opcode_cases (w : InstWord) :
    w.extractLsb' 0 4 = 0b0000#4 \/ w.extractLsb' 0 4 = 0b0001#4
      \/ w.extractLsb' 0 4 = 0b0010#4 \/ w.extractLsb' 0 4 = 0b0011#4
      \/ w.extractLsb' 0 4 = 0b0100#4 \/ w.extractLsb' 0 4 = 0b0101#4
      \/ w.extractLsb' 0 4 = 0b0110#4 \/ w.extractLsb' 0 4 = 0b0111#4
      \/ w.extractLsb' 0 4 = 0b1000#4 \/ w.extractLsb' 0 4 = 0b1001#4
      \/ w.extractLsb' 0 4 = 0b1010#4 \/ w.extractLsb' 0 4 = 0b1011#4
      \/ w.extractLsb' 0 4 = 0b1100#4 \/ w.extractLsb' 0 4 = 0b1101#4
      \/ w.extractLsb' 0 4 = 0b1110#4 \/ w.extractLsb' 0 4 = 0b1111#4 := by
  bv_decide


-- If the decode of an InstWord is an Inst, then the encode of the Inst is the InstWord. Each arm
-- recovers the instruction from the fields of w, rewrites every register field back into the
-- nibble it was decoded from, and is left with the join of the four nibbles of w.
theorem Inst.enc_dec (w : InstWord) (i : Inst) (h : Inst.dec w = some i) : i.enc = w := by
  simp only [Inst.dec] at h
  split at h
  -- 0b0000, HALT and SYSCALL, whose reserved field is zero and whose target bit picks the two
  -- apart
  next =>
    split at h
    next => simp at h
    next =>
      split at h
      next =>
        simp [GpReg.dec_eq_decD] at h
        subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
      next =>
        simp [GpReg.dec_eq_decD] at h
        subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0001, ADD
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0010, ADC
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0011, SUB
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0100, SBB
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0101, AND
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0110, OR
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b0111, XOR
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b1000, RRC, whose reserved nibble is the zero that Inst.enc writes there
  next =>
    split at h
    next => simp at h
    next =>
      simp [GpReg.dec_eq_decD] at h
      subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b1001, SET
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b1010, LD
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b1011, ST
  next =>
    simp [GpReg.dec_eq_decD] at h
    subst h; simp only [Inst.enc, GpReg.enc_decD]; bv_decide
  -- 0b1100, RSR, whose special register field can fail to decode
  next =>
    cases hcd : CsrReg.dec (w.extractLsb' 12 4) with
    | none   => simp [GpReg.dec_eq_decD, hcd] at h
    | some c =>
      simp [GpReg.dec_eq_decD, hcd] at h
      subst h
      simp only [Inst.enc, GpReg.enc_decD, CsrReg.enc_dec _ _ hcd]
      bv_decide
  -- 0b1101, WSR, whose special register field can fail to decode
  next =>
    cases hcd : CsrReg.dec (w.extractLsb' 12 4) with
    | none   => simp [GpReg.dec_eq_decD, hcd] at h
    | some c =>
      simp [GpReg.dec_eq_decD, hcd] at h
      subst h
      simp only [Inst.enc, GpReg.enc_decD, CsrReg.enc_dec _ _ hcd]
      bv_decide
  -- 0b1110, JAL, whose target bit is the zero that Inst.enc writes there and whose comparison can
  -- fail to decode
  next =>
    split at h
    next => simp at h
    next =>
      cases hfd : CmpFlag.dec (w.extractLsb' 12 3) with
      | none   => simp [GpReg.dec_eq_decD, hfd] at h
      | some f =>
        simp [GpReg.dec_eq_decD, hfd] at h
        subst h
        simp only [Inst.enc, GpReg.enc_decD, CmpFlag.enc_dec _ _ hfd]
        bv_decide
  -- 0b1111, JMP and the special register variant of JMP, which the target bit picks apart
  next =>
    split at h
    next =>
      split at h
      next => simp at h
      next =>
        cases hcd : CsrReg.dec (w.extractLsb' 4 4) with
        | none   => simp [hcd] at h
        | some c =>
          cases hfd : CmpFlag.dec (w.extractLsb' 12 3) with
          | none   => simp [hcd, hfd] at h
          | some f =>
            simp [hcd, hfd] at h
            subst h
            simp only [Inst.enc, CsrReg.enc_dec _ _ hcd, CmpFlag.enc_dec _ _ hfd]
            bv_decide
    next =>
      cases hfd : CmpFlag.dec (w.extractLsb' 12 3) with
      | none   => simp [GpReg.dec_eq_decD, hfd] at h
      | some f =>
        simp [GpReg.dec_eq_decD, hfd] at h
        subst h
        simp only [Inst.enc, GpReg.enc_decD, CmpFlag.enc_dec _ _ hfd]
        bv_decide
  -- The arm no opcode nibble reaches
  next => simp at h


-- If two Inst encodings are the same, then the two Inst constructors are the same
theorem Inst.enc_inj (i1 : Inst) (i2 : Inst) (h : i1.enc = i2.enc) : i1 = i2 := by
  have h1 : Inst.dec i1.enc = some i1 := Inst.dec_enc i1
  rw [h, Inst.dec_enc i2] at h1
  exact (Option.some.inj h1).symm


-- A HALT instruction with a non-zero reserved field does not decode
theorem Inst.dec_reserved_halt (w : InstWord)
    (h0 : w.extractLsb' 0 4 = 0b0000#4) (h : Ne (w.extractLsb' 8 7) 0#7) :
    Inst.dec w = none := by
  simp [Inst.dec, h0, h]
  rfl


-- A RRC instruction with a non-zero reserved field does not decode
theorem Inst.dec_reserved_rrc (w : InstWord)
    (h0 : w.extractLsb' 0 4 = 0b1000#4) (h : Ne (w.extractLsb' 12 4) 0b0000#4) :
    Inst.dec w = none := by
  simp [Inst.dec, h0, h]
  rfl


-- A JAL instruction with a non-zero reserved field does not decode
theorem Inst.dec_reserved_jal (w : InstWord)
    (h0 : w.extractLsb' 0 4 = 0b1110#4) (h : w.getLsbD 15 = true) :
    Inst.dec w = none := by
  simp [Inst.dec, h0, h]
  rfl


-- A JMP instruction (CSR variant) with a non-zero reserved field does not decode
theorem Inst.dec_reserved_jmpCsr (w : InstWord)
    (h0 : w.extractLsb' 0 4 = 0b1111#4) (h1 : w.getLsbD 15 = true)
    (h : Ne (w.extractLsb' 8 4) 0b0000#4) :
    Inst.dec w = none := by
  simp [Inst.dec, h0, h1, h]
  rfl


-- The CAUSE value recorded by a HALT is the RS register in the high byte and 0x01 in the low byte
theorem CsrCause.haltInstruction_enc (rs : DataWord) :
    (CsrCause.haltInstruction rs).enc = (rs.setWidth 16 <<< 8) ||| 0x0001#16 := by
  simp only [CsrCause.enc, CsrCause.haltInstruction, FaultKind.enc]
  bv_decide


-- If two FaultKind encodings are the same, then the two FaultKind constructors are the same
theorem FaultKind.enc_inj (k1 : FaultKind) (k2 : FaultKind) (h : k1.enc = k2.enc) : k1 = k2 := by
  cases k1 <;> cases k2 <;> simp_all [FaultKind.enc]


-- Every FaultKind splits into the Reason and Type pair that the manual assigns it
theorem FaultKind.reason_type_table (k : FaultKind) :
    (k.enc.extractLsb' 0 3, k.enc.extractLsb' 3 3)
      = match k with
        | .nothing                  => (0b000#3, 0b000#3)
        | .haltInstruction          => (0b001#3, 0b000#3)
        | .illegalInstruction       => (0b001#3, 0b001#3)
        | .privilegedInstruction    => (0b001#3, 0b010#3)
        | .addressOverflow          => (0b010#3, 0b000#3)
        | .pcOverflow               => (0b010#3, 0b001#3)
        | .privilegedAccess         => (0b010#3, 0b010#3)
        | .vectorWrite              => (0b010#3, 0b011#3)
        | .improperReturnContext    => (0b011#3, 0b000#3)
        | .conditionalContextReturn => (0b011#3, 0b001#3)
        | .improperSystemCall       => (0b011#3, 0b010#3) := by
  cases k <;> decide


-- Every FaultKind other than nothing has a non-zero Reason field
theorem FaultKind.reason_ne_zero (k : FaultKind) (h : Ne k FaultKind.nothing) :
    Ne (k.enc.extractLsb' 0 3) 0b000#3 := by
  cases k
  case nothing => exact absurd rfl h
  all_goals decide


-- Every CAUSE value the processor records leaves the reserved CAUSE bits zero
theorem CsrCause.enc_respects_writeMask (c : CsrCause) :
    c.enc &&& ~~~ CsrReg.cause.writeMask = 0x0000#16 := by
  simp only [CsrCause.enc, CsrReg.writeMask]
  bv_decide


-- The extended status of an improper return context holds the two context bits and the target CSR
theorem CsrCause.improperReturnContext_status (inSyscall : Bool) (inInterrupt : Bool)
    (csr : CsrReg) :
    let s := (CsrCause.improperReturnContext inSyscall inInterrupt csr).status
    s.getLsbD 0 = inSyscall
      /\ s.getLsbD 1 = inInterrupt
      /\ s.getLsbD 2 = false
      /\ s.getLsbD 3 = false
      /\ s.extractLsb' 4 4 = csr.enc := by
  cases inSyscall <;> cases inInterrupt <;> cases csr <;> decide


-- The extended status of a conditional context return holds the comparison and the target CSR
theorem CsrCause.conditionalContextReturn_status (cmp : CmpFlag) (csr : CsrReg) :
    let s := (CsrCause.conditionalContextReturn cmp csr).status
    s.extractLsb' 0 3 = cmp.enc
      /\ s.getLsbD 3 = false
      /\ s.extractLsb' 4 4 = csr.enc := by
  cases cmp <;> cases csr <;> decide


-- The extended status of a vector write holds the low nibble of the address the store would have
-- written, which names the vector it tried to overwrite
theorem CsrCause.vectorWrite_status (addr : AddrWord) :
    let s := (CsrCause.vectorWrite addr).status
    s.extractLsb' 0 4 = addr.extractLsb' 0 4
      /\ s.extractLsb' 4 4 = 0b0000#4 := by
  simp only [CsrCause.vectorWrite]
  bv_decide


-- The extended status of an improper system call holds the two context bits and nothing else
theorem CsrCause.improperSystemCall_status (inSyscall : Bool) (inInterrupt : Bool) :
    let s := (CsrCause.improperSystemCall inSyscall inInterrupt).status
    s.getLsbD 0 = inSyscall
      /\ s.getLsbD 1 = inInterrupt
      /\ s.extractLsb' 2 6 = 0b000000#6 := by
  cases inSyscall <;> cases inInterrupt <;> decide


-- The decode of the encode of a CsrFlags is that CsrFlags
theorem CsrFlags.dec_enc (f : CsrFlags) : CsrFlags.dec f.enc = f := by
  cases f with
  | mk z c n v => cases z <;> cases c <;> cases n <;> cases v <;> decide


-- The encode of the decode of a SpecWord with no reserved bit set is that SpecWord
theorem CsrFlags.enc_dec (n : SpecWord) (h : n &&& ~~~ CsrReg.flags.writeMask = 0x0000#16) :
    (CsrFlags.dec n).enc = n := by
  simp only [CsrReg.writeMask] at h
  simp only [CsrFlags.enc, CsrFlags.dec]
  bv_decide


-- A CsrFlags encodes Z, C, N, and V into bits 0 to 3 and leaves every other bit zero
theorem CsrFlags.enc_bits (f : CsrFlags) :
    f.enc.getLsbD 0 = f.z
      /\ f.enc.getLsbD 1 = f.c
      /\ f.enc.getLsbD 2 = f.n
      /\ f.enc.getLsbD 3 = f.v
      /\ f.enc.extractLsb' 4 12 = 0#12 := by
  cases f with
  | mk z c n v => cases z <;> cases c <;> cases n <;> cases v <;> decide


-- The decode of the encode of a CsrExtns is that CsrExtns
theorem CsrExtns.dec_enc (e : CsrExtns) : CsrExtns.dec e.enc = e := by
  cases e with
  | mk dynamichw syscall privilege interrupt =>
    cases dynamichw <;> cases syscall <;> cases privilege <;> cases interrupt <;> decide


-- A CsrExtns encodes its four extension bits into bits 0 to 3 and leaves every other bit zero
theorem CsrExtns.enc_bits (e : CsrExtns) :
    e.enc.getLsbD 0 = e.dynamichw
      /\ e.enc.getLsbD 1 = e.syscall
      /\ e.enc.getLsbD 2 = e.privilege
      /\ e.enc.getLsbD 3 = e.interrupt
      /\ e.enc.extractLsb' 4 12 = 0#12 := by
  cases e with
  | mk dynamichw syscall privilege interrupt =>
    cases dynamichw <;> cases syscall <;> cases privilege <;> cases interrupt <;> decide


-- Each CmpFlag evaluates to the boolean formula over Z, C, N, and V that the manual gives for it
theorem CsrFlags.eval_table (f : CsrFlags) :
    f.eval .always       = true
      /\ f.eval .equal        = f.z
      /\ f.eval .notEqual     = ! f.z
      /\ f.eval .below        = ! f.c
      /\ f.eval .less         = (f.n ^^ f.v)
      /\ f.eval .greaterEqual = ! (f.n ^^ f.v)
      /\ f.eval .aboveEqual   = f.c := by
  cases f with
  | mk z c n v => cases z <;> cases c <;> cases n <;> cases v <;> decide


end MisaSpec

import MisaProof.Wf


-- Destination register binders are named rd after the architecture's operand names, which collides
-- with the GpReg.rd constructor
set_option linter.constructorNameAsVariable false


namespace MisaSpec


-- The top bit of the nine bit adder output is the carry out of an eight bit addition
theorem bit8_eq_ge (x : BitVec 9) : x.getLsbD 8 = decide (256 <= x.toNat) := by
  have h : x.getLsbD 8 = BitVec.ule 256#9 x := by bv_decide
  rw [h]
  simp [BitVec.ule]


-- The top bit of a data word is its sign as a two's complement integer
theorem bit7_eq_neg (x : DataWord) : x.getLsbD 7 = decide (x.toInt < 0) := by
  have h : x.getLsbD 7 = BitVec.slt x 0#8 := by bv_decide
  rw [h]
  simp [BitVec.slt]


-- The top bit of a data word is set exactly when its unsigned value reaches the halfway point
theorem bit7_eq_ge (x : DataWord) : x.getLsbD 7 = decide (128 <= x.toNat) := by
  have h : x.getLsbD 7 = BitVec.ule 128#8 x := by bv_decide
  rw [h]
  simp [BitVec.ule]


-- A data word's signed value is its unsigned value, less the modulus when its top bit is set
set_option maxRecDepth 4096 in
theorem toInt_eq_toNat_sub (x : DataWord) :
    x.toInt = (x.toNat : Int) - (if x.getLsbD 7 then 256 else 0) :=
  of_decide_eq_true (forall_dataWord
    (fun y => decide (y.toInt = (y.toNat : Int) - (if y.getLsbD 7 then 256 else 0)))
    (by decide) x)


-- The adder result is the eight bit sum of its two operands and its carry in
theorem aluAdd_val (a : DataWord) (b : DataWord) (cin : Bool) :
    (aluAdd a b cin).1 = a + b + (if cin then 1#8 else 0#8) := by
  cases cin <;> simp [aluAdd] <;> bv_decide


-- The C flag is set when the unsigned sum of the adder operands and carry in does not fit in eight
-- bits
theorem aluAdd_c (a : DataWord) (b : DataWord) (cin : Bool) :
    (aluAdd a b cin).2.c = decide (256 <= a.toNat + b.toNat + cin.toNat) := by
  have ha : a.toNat < 256 := a.isLt
  have hb : b.toNat < 256 := b.isLt
  simp only [aluAdd, bit8_eq_ge, decide_eq_decide]
  cases cin <;> simp [BitVec.toNat_add, BitVec.toNat_setWidth] <;> omega


-- The N flag is set when the adder result is negative as a two's complement integer
theorem aluAdd_n (a : DataWord) (b : DataWord) (cin : Bool) :
    (aluAdd a b cin).2.n = decide ((aluAdd a b cin).1.toInt < 0) := by
  simp only [aluAdd]
  exact bit7_eq_neg _


-- The V flag is set when the signed sum of the adder operands and carry in leaves the range of an
-- eight bit two's complement integer
theorem aluAdd_v (a : DataWord) (b : DataWord) (cin : Bool) :
    (aluAdd a b cin).2.v
      = (decide (a.toInt + b.toInt + (cin.toNat : Int) < -128)
         || decide (127 < a.toInt + b.toInt + (cin.toNat : Int))) := by
  have ha : a.toNat < 256 := a.isLt
  have hb : b.toNat < 256 := b.isLt
  have hc : cin.toNat = 0 \/ cin.toNat = 1 := by cases cin <;> simp
  have hA : a.toInt = (a.toNat : Int) - (if a.getLsbD 7 then 256 else 0) := toInt_eq_toNat_sub a
  have hB : b.toInt = (b.toNat : Int) - (if b.getLsbD 7 then 256 else 0) := toInt_eq_toNat_sub b
  have hres : (aluAdd a b cin).1.toNat = (a.toNat + b.toNat + cin.toNat) % 256 := by
    cases cin <;> simp [aluAdd, BitVec.toNat_add, BitVec.toNat_setWidth] <;> omega
  have hpa : a.getLsbD 7 = decide (128 <= a.toNat) := bit7_eq_ge a
  have hpb : b.getLsbD 7 = decide (128 <= b.toNat) := bit7_eq_ge b
  have hpr : (aluAdd a b cin).1.getLsbD 7
      = decide (128 <= (a.toNat + b.toNat + cin.toNat) % 256) := by
    rw [bit7_eq_ge, hres]
  have hv : (aluAdd a b cin).2.v
      = ((a.getLsbD 7 == b.getLsbD 7) && (b.getLsbD 7 != (aluAdd a b cin).1.getLsbD 7)) := rfl
  rw [hv, hA, hB, hpa, hpb, hpr]
  by_cases h1 : 128 <= a.toNat
  all_goals by_cases h2 : 128 <= b.toNat
  all_goals by_cases h3 : 128 <= (a.toNat + b.toNat + cin.toNat) % 256
  all_goals simp [h1, h2, h3] <;> omega


-- subFlags is the adder applied to the first operand, the complement of the second, and a carry in
-- of one
theorem subFlags_eq (a : DataWord) (b : DataWord) : subFlags a b = (aluAdd a (~~~ b) true).2 :=
  rfl


-- Adding the complement of the second operand with a carry in of one subtracts it
theorem sub_val (a : DataWord) (b : DataWord) : (aluAdd a (~~~ b) true).1 = a - b := by
  simp [aluAdd] <;> bv_decide


-- The C flag after a SUB is the unsigned greater-or-equal predicate on its operands, so it is a
-- carry and never a borrow
theorem sub_c_is_unsigned_ge (a : DataWord) (b : DataWord) :
    (subFlags a b).c = BitVec.ule b a := by
  simp [subFlags, aluAdd] <;> bv_decide


-- The Z flag after a SUB is set when its operands are equal
theorem sub_z_is_eq (a : DataWord) (b : DataWord) : (subFlags a b).z = (a == b) := by
  simp [subFlags, aluAdd] <;> bv_decide


-- A SUB leaves FLAGS holding subFlags of the two source registers
theorem executeSub_flags (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    CsrFlags.dec ((executeSub q rd rs1 rs2).special CsrReg.flags)
      = subFlags (q.register rs1) (q.register rs2) := by
  simp [executeSub, State.next_flags, CsrFlags.dec_enc, subFlags]


-- Every comparison after a SUB evaluates subFlags of the two source registers
theorem executeSub_check (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag) :
    (executeSub q rd rs1 rs2).check cmp
      = (subFlags (q.register rs1) (q.register rs2)).eval cmp := by
  simp [executeSub, State.next_check, subFlags]


-- EQUAL and NOT_EQUAL after a CMP decide equality of its operands, which is the assumption the
-- manual makes about comparing registers with SUB R0 RS1 RS2
theorem cmp_check_equality (q : State) (rs1 : GpReg) (rs2 : GpReg) :
    let a := q.register rs1
    let b := q.register rs2
    let q' := executeSub q GpReg.r0 rs1 rs2
    q'.check .equal     = (a == b)
      /\ q'.check .notEqual = (a != b) := by
  simp only [executeSub_check, subFlags, aluAdd, CsrFlags.eval]
  refine And.intro ?_ ?_
  all_goals bv_decide


-- LESS and GREATER_EQUAL after a CMP decide the signed relations on its operands
theorem cmp_check_signed (q : State) (rs1 : GpReg) (rs2 : GpReg) :
    let a := q.register rs1
    let b := q.register rs2
    let q' := executeSub q GpReg.r0 rs1 rs2
    q'.check .less        = BitVec.slt a b
      /\ q'.check .greaterEqual = BitVec.sle b a := by
  simp only [executeSub_check, subFlags, aluAdd, CsrFlags.eval]
  refine And.intro ?_ ?_
  all_goals bv_decide


-- BELOW and ABOVE_EQUAL after a CMP decide the unsigned relations on its operands, so the unsigned
-- predicate carried in C is reachable from a conditional jump
theorem cmp_check_unsigned (q : State) (rs1 : GpReg) (rs2 : GpReg) :
    let a := q.register rs1
    let b := q.register rs2
    let q' := executeSub q GpReg.r0 rs1 rs2
    q'.check .below       = BitVec.ult a b
      /\ q'.check .aboveEqual = BitVec.ule b a := by
  simp only [executeSub_check, subFlags, aluAdd, CsrFlags.eval]
  refine And.intro ?_ ?_
  all_goals bv_decide


-- The comparison set is closed under negation, so no branch condition has to be inverted by
-- restructuring code
theorem cmpFlag_closed_under_negation (f : CsrFlags) :
    f.eval .less  = ! f.eval .greaterEqual
      /\ f.eval .below = ! f.eval .aboveEqual
      /\ f.eval .equal = ! f.eval .notEqual := by
  cases f with
  | mk z c n v => cases z <;> cases c <;> cases n <;> cases v <;> decide


-- Only four of the eight ordered relations have a comparison flag, but reversing the operand order
-- of the CMP reaches the other four, so every signed and unsigned ordering is decidable
theorem cmpFlag_covers_orderings (a : DataWord) (b : DataWord) :
    (subFlags a b).eval .less         = BitVec.slt a b
      /\ (subFlags a b).eval .greaterEqual = BitVec.sle b a
      /\ (subFlags b a).eval .less         = BitVec.slt b a
      /\ (subFlags b a).eval .greaterEqual = BitVec.sle a b
      /\ (subFlags a b).eval .below        = BitVec.ult a b
      /\ (subFlags a b).eval .aboveEqual   = BitVec.ule b a
      /\ (subFlags b a).eval .below        = BitVec.ult b a
      /\ (subFlags b a).eval .aboveEqual   = BitVec.ule a b := by
  simp only [subFlags, aluAdd, CsrFlags.eval]
  refine And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_
    (And.intro ?_ (And.intro ?_ ?_))))))
  all_goals bv_decide


-- An AND sets Z and N from its result and clears C and V
theorem executeAnd_flags (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    let res := q.register rs1 &&& q.register rs2
    let f := CsrFlags.dec ((executeAnd q rd rs1 rs2).special CsrReg.flags)
    f.z = (res == 0x00#8) /\ f.c = false /\ f.n = res.getLsbD 7 /\ f.v = false := by
  simp [executeAnd, State.next_flags, CsrFlags.dec_enc]


-- An OR sets Z and N from its result and clears C and V
theorem executeOr_flags (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    let res := q.register rs1 ||| q.register rs2
    let f := CsrFlags.dec ((executeOr q rd rs1 rs2).special CsrReg.flags)
    f.z = (res == 0x00#8) /\ f.c = false /\ f.n = res.getLsbD 7 /\ f.v = false := by
  simp [executeOr, State.next_flags, CsrFlags.dec_enc]


-- A XOR sets Z and N from its result and clears C and V
theorem executeXor_flags (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    let res := q.register rs1 ^^^ q.register rs2
    let f := CsrFlags.dec ((executeXor q rd rs1 rs2).special CsrReg.flags)
    f.z = (res == 0x00#8) /\ f.c = false /\ f.n = res.getLsbD 7 /\ f.v = false := by
  simp [executeXor, State.next_flags, CsrFlags.dec_enc]


-- A RRC stores the low eight bits of the nine bit value formed by C and the source shifted right
-- by one
theorem executeRrc_val (q : State) (rd : GpReg) (rs : GpReg) (h : Ne rd GpReg.r0) :
    let cin := (CsrFlags.dec (q.special CsrReg.flags)).c
    (executeRrc q rd rs).register rd
      = (BitVec.ofBool cin ++ q.register rs).extractLsb' 1 8 := by
  cases hc : (CsrFlags.dec (q.special CsrReg.flags)).c
  all_goals
    simp [executeRrc, hc, State.next_register,
          State.writeReg_self _ _ _ h] <;> bv_decide


-- A RRC sets C from bit 0 of its source register rather than from the adder
theorem executeRrc_c (q : State) (rd : GpReg) (rs : GpReg) :
    (CsrFlags.dec ((executeRrc q rd rs).special CsrReg.flags)).c = (q.register rs).getLsbD 0 := by
  simp [executeRrc, State.next_flags, CsrFlags.dec_enc]


-- An ADD sets the same flags whatever its destination register, so the flags come from the ALU
-- output and not from a read back of RD
theorem executeAdd_flags_independent_of_rd (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    (executeAdd q rd rs1 rs2).special CsrReg.flags
      = (executeAdd q GpReg.r0 rs1 rs2).special CsrReg.flags := by
  simp [executeAdd, State.next_flags]


-- A SUB sets the same flags whatever its destination register, which is what makes CMP work
theorem executeSub_flags_independent_of_rd (q : State) (rd : GpReg) (rs1 : GpReg) (rs2 : GpReg) :
    (executeSub q rd rs1 rs2).special CsrReg.flags
      = (executeSub q GpReg.r0 rs1 rs2).special CsrReg.flags := by
  simp [executeSub, State.next_flags]


end MisaSpec

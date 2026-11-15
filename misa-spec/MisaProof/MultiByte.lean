import MisaProof.Alu


namespace MisaSpec


-- An ADD followed by an ADC on the high bytes computes the sixteen bit sum, and leaves C holding
-- the carry out of bit 15
theorem add2_correct (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    let c1 := (aluAdd aLo bLo false).2.c
    let lo := (aluAdd aLo bLo false).1
    let hi := (aluAdd aHi bHi c1).1
    let c2 := (aluAdd aHi bHi c1).2.c
    (hi ++ lo) = (aHi ++ aLo) + (bHi ++ bLo)
      /\ c2 = decide (65536 <= (aHi ++ aLo).toNat + (bHi ++ bLo).toNat) := by
  have hx : (aHi ++ aLo).toNat < 65536 := (aHi ++ aLo).isLt
  have hy : (bHi ++ bLo).toNat < 65536 := (bHi ++ bLo).isLt
  refine And.intro ?_ ?_
  next =>
    simp only [aluAdd]
    bv_decide
  next =>
    have h1 : (aluAdd aHi bHi (aluAdd aLo bLo false).2.c).2.c
        = ((aHi ++ aLo).setWidth 17 + (bHi ++ bLo).setWidth 17).getLsbD 16 := by
      simp only [aluAdd]
      bv_decide
    rw [h1, bit16_eq_ge, decide_eq_decide]
    simp [BitVec.toNat_add, BitVec.toNat_setWidth]
    omega


-- A SUB followed by a SBB on the high bytes computes the sixteen bit difference
theorem sub2_correct (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    let c1 := (subFlags aLo bLo).c
    let lo := (aluAdd aLo (~~~ bLo) true).1
    let hi := (aluAdd aHi (~~~ bHi) c1).1
    (hi ++ lo) = (aHi ++ aLo) - (bHi ++ bLo) := by
  simp only [subFlags, aluAdd]
  bv_decide


-- The C flag after a SUB2 is the unsigned greater-or-equal predicate on the sixteen bit operands
theorem sub2_c_is_unsigned_ge (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    let c1 := (subFlags aLo bLo).c
    (aluAdd aHi (~~~ bHi) c1).2.c = BitVec.ule (bHi ++ bLo) (aHi ++ aLo) := by
  simp only [subFlags, aluAdd]
  bv_decide


-- The four ordered comparisons after a SUB2 decide the signed relations on the sixteen bit
-- operands. LESS and GREATER_EQUAL do hold, because N and V after the high byte SBB describe the
-- full difference. EXPECTED TO FAIL: GREATER and LESS_EQUAL also read Z, which reflects the high
-- byte alone, so subtracting 0xFFFF from 0x00FE leaves a high byte of zero and reads as not
-- greater. Needs the decision on whether ADC and SBB accumulate Z.
theorem sub2_check_signed16 (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    let c1 := (subFlags aLo bLo).c
    let f2 := (aluAdd aHi (~~~ bHi) c1).2
    f2.eval .less         = BitVec.slt (aHi ++ aLo) (bHi ++ bLo)
      /\ f2.eval .greaterEqual = BitVec.sle (bHi ++ bLo) (aHi ++ aLo)
      /\ f2.eval .greater      = BitVec.slt (bHi ++ bLo) (aHi ++ aLo)
      /\ f2.eval .lessEqual    = BitVec.sle (aHi ++ aLo) (bHi ++ bLo) :=
  sorry


-- EQUAL after a SUB2 does not decide sixteen bit equality, because Z reflects the high byte alone.
-- Subtracting 0x0101 from 0x0102 leaves a high byte of zero and so reads as equal.
theorem sub2_check_equal_unsound :
    let aHi : DataWord := 0x01#8
    let aLo : DataWord := 0x02#8
    let bHi : DataWord := 0x01#8
    let bLo : DataWord := 0x01#8
    let c1 := (subFlags aLo bLo).c
    let f2 := (aluAdd aHi (~~~ bHi) c1).2
    f2.eval .equal = true /\ Ne (aHi ++ aLo) (bHi ++ bLo) := by
  decide


-- A RRC on the high byte followed by a RRC on the low byte shifts the seventeen bit value formed
-- by C and the two source bytes right by one
theorem rrc2_correct (cin : Bool) (aHi : DataWord) (aLo : DataWord) :
    let hi   := (if cin then 0x80#8 else 0x00#8) ||| (aHi >>> 1)
    let cMid := aHi.getLsbD 0
    let lo   := (if cMid then 0x80#8 else 0x00#8) ||| (aLo >>> 1)
    (hi ++ lo) = (BitVec.ofBool cin ++ aHi ++ aLo).extractLsb' 1 16 := by
  cases cin <;> cases hm : aHi.getLsbD 0 <;> simp <;> bv_decide


-- The carry left by a RRC2 is the bit shifted out of the seventeen bit value, so the macro chains
-- into a wider shift
theorem rrc2_carry (cin : Bool) (aHi : DataWord) (aLo : DataWord) :
    aLo.getLsbD 0 = (BitVec.ofBool cin ++ aHi ++ aLo).getLsbD 0 := by
  bv_decide


-- Two byte-wise ANDs compute the sixteen bit AND
theorem and2_correct (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    ((aHi &&& bHi) ++ (aLo &&& bLo)) = (aHi ++ aLo) &&& (bHi ++ bLo) := by
  bv_decide


-- Two byte-wise ORs compute the sixteen bit OR
theorem or2_correct (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    ((aHi ||| bHi) ++ (aLo ||| bLo)) = (aHi ++ aLo) ||| (bHi ++ bLo) := by
  bv_decide


-- Two byte-wise XORs compute the sixteen bit XOR
theorem xor2_correct (aHi : DataWord) (aLo : DataWord) (bHi : DataWord) (bLo : DataWord) :
    ((aHi ^^^ bHi) ++ (aLo ^^^ bLo)) = (aHi ++ aLo) ^^^ (bHi ++ bLo) := by
  bv_decide


end MisaSpec

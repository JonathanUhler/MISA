import MisaProof.MemMap


/-
The memory protection unit.

MPUCF is a window onto four region registers rather than a register in its own right, which makes
three things worth establishing: that the window encoding round-trips, that the cheap comparison
the model performs is the page span the manual describes, and that nothing but a WSR on MPUCF can
move a region. The last is what lets the User Mode permission check in a later layer rely on the
regions being whatever software last configured.
-/


namespace MisaSpec


-- The MPUCF window round-trips: every field of a region survives being encoded and read back,
-- which is what makes a read report the region information last written
theorem MpuRegion.dec_enc (reg : MpuRegion) : MpuRegion.dec reg.enc = reg := by
  cases reg
  simp only [MpuRegion.enc, MpuRegion.dec, MpuRegion.mk.injEq]
  bv_decide


-- Every bit of the window is a field, so the whole 16-bit value is recovered by a read
theorem MpuRegion.enc_dec (n : SpecWord) : (MpuRegion.dec n).enc = n := by
  simp only [MpuRegion.enc, MpuRegion.dec]
  bv_decide


-- The comparison the model performs is exactly the span the manual describes: the region covers
-- the 2 ^ Size pages starting at the naturally aligned base page B = (Base >> Size) << Size. The
-- upper bound is B with the low Size bits set, which is B + 2 ^ Size - 1.
theorem MpuRegion.contains_iff_span (reg : MpuRegion) (a : AddrWord) :
    reg.contains a
      = (decide (((reg.base >>> reg.size.setWidth 8) <<< reg.size.setWidth 8) <= pageOf a)
         && decide (pageOf a <= ((reg.base >>> reg.size.setWidth 8) <<< reg.size.setWidth 8)
                      ||| ((1#8 <<< reg.size.setWidth 8) - 1#8))) := by
  simp only [MpuRegion.contains]
  bv_decide


-- The low Size bits of Base take no part in the comparison, so they are ignored rather than
-- illegal and every Base value describes a region
theorem MpuRegion.contains_ignores_base_low_bits (reg1 : MpuRegion) (reg2 : MpuRegion)
    (a : AddrWord) (hs : reg1.size = reg2.size)
    (hb : reg1.base >>> reg1.size.setWidth 8 = reg2.base >>> reg2.size.setWidth 8) :
    reg1.contains a = reg2.contains a := by
  simp only [MpuRegion.contains]
  rw [hb, hs]


-- No region extends past the last page. Size reaches 7, so a region spans at most 128 of the 256
-- pages, and the span's upper bound never wraps past 0xFF: every region is a contiguous run.
theorem MpuRegion.span_no_wrap (reg : MpuRegion) :
    ((reg.base >>> reg.size.setWidth 8) <<< reg.size.setWidth 8)
      <= (((reg.base >>> reg.size.setWidth 8) <<< reg.size.setWidth 8)
           ||| ((1#8 <<< reg.size.setWidth 8) - 1#8)) := by
  bv_decide


-- Overlap resolves to the lowest index that contains the page, and an address no region contains
-- is granted nothing
theorem Mpu.caps_priority (regs : BitVec 2 -> SpecWord) (a : AddrWord) :
    Mpu.caps regs a
      = if (MpuRegion.dec (regs 0b00#2)).contains a then (MpuRegion.dec (regs 0b00#2)).caps
        else if (MpuRegion.dec (regs 0b01#2)).contains a then (MpuRegion.dec (regs 0b01#2)).caps
        else if (MpuRegion.dec (regs 0b10#2)).contains a then (MpuRegion.dec (regs 0b10#2)).caps
        else if (MpuRegion.dec (regs 0b11#2)).contains a then (MpuRegion.dec (regs 0b11#2)).caps
        else MemCaps.empty := by
  simp only [Mpu.caps, MpuRegion.indices, List.find?]
  repeat (any_goals split)
  all_goals simp_all


-- A configuration in which every region register is clear grants nothing anywhere, which is the
-- state the four regions are required to reset into
theorem Mpu.caps_zero (regs : BitVec 2 -> SpecWord) (h : forall i : BitVec 2, regs i = 0x0000#16)
    (a : AddrWord) : Mpu.caps regs a = MemCaps.empty := by
  rw [Mpu.caps_priority]
  simp only [h, MpuRegion.dec, MpuRegion.caps]
  repeat (any_goals split)
  all_goals rfl


-- Reset leaves the MPU granting no User Mode access to any address, whether or not the Privilege
-- Extension is enabled out of reset
theorem State.first_mpu_grants_nothing (bytes : ByteArray) (extns : SpecWord) (map : MemMap)
    (a : AddrWord) :
    Mpu.caps (State.first bytes extns map).mpuRegion a = MemCaps.empty :=
  Mpu.caps_zero _ (fun _ => rfl) a


-- MPUCF retains every bit of what is written to it, so the window masks nothing away
theorem mpucf_writeMask_id (v : SpecWord) : v &&& CsrReg.mpucf.writeMask = v := by
  simp only [CsrReg.writeMask]
  bv_decide


-- A WSR on MPUCF lands in the region its Index field selects and leaves the other three alone
theorem State.writeSpecial_mpucf_region (q : State) (v : SpecWord) (i : BitVec 2) :
    (q.writeSpecial CsrReg.mpucf v).mpuRegion i
      = if i == (MpuRegion.dec v).index then v else q.mpuRegion i := by
  simp only [State.writeSpecial, mpucf_writeMask_id]


-- A WSR on MPUCF reports back the word it wrote, which carries both the region information and
-- the index it was written to
theorem State.writeSpecial_mpucf_readback (q : State) (v : SpecWord) :
    (q.writeSpecial CsrReg.mpucf v).special CsrReg.mpucf = v := by
  simp only [State.writeSpecial, mpucf_writeMask_id]
  simp


-- The written word is the region the selected index now holds, so a read after a write reports
-- that region rather than a stale one
theorem State.writeSpecial_mpucf_selected (q : State) (v : SpecWord) :
    (q.writeSpecial CsrReg.mpucf v).mpuRegion (MpuRegion.dec v).index = v := by
  rw [State.writeSpecial_mpucf_region]
  simp


-- No special register write other than one naming MPUCF can move an MPU region
theorem State.writeSpecial_region_other (q : State) (c : CsrReg) (v : SpecWord) (i : BitVec 2)
    (h : Ne c CsrReg.mpucf) : (q.writeSpecial c v).mpuRegion i = q.mpuRegion i := by
  cases c
  case mpucf => exact absurd rfl h
  case extns => simp only [State.writeSpecial]; split <;> rfl
  all_goals rfl


end MisaSpec

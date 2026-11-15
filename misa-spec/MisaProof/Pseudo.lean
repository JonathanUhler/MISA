import MisaProof.Alu


namespace MisaSpec


-- A NOP only advances the program counter
theorem nop_is_pc_increment (q : State) (imm : DataWord) :
    executeSet q GpReg.r0 imm = State.next q :=
  sorry


-- A MOV copies its source register into its destination register
theorem mov_result (q : State) (rd : GpReg) (rs : GpReg) (h : Ne rd GpReg.r0) :
    (executeOr q rd rs GpReg.r0).register rd = q.register rs :=
  sorry


-- A MOV updates FLAGS, unlike a NOP, so it cannot be placed between a comparison and its branch
theorem mov_clobbers_flags (q : State) (rd : GpReg) (rs : GpReg) :
    (executeOr q rd rs GpReg.r0).check .equal = (q.register rs == 0x00#8) :=
  sorry


-- A CMP leaves every register unchanged
theorem cmp_preserves_registers (q : State) (rs1 : GpReg) (rs2 : GpReg) (r : GpReg) :
    (executeSub q GpReg.r0 rs1 rs2).register r = q.register r :=
  sorry


-- A CMP leaves memory unchanged
theorem cmp_preserves_memory (q : State) (rs1 : GpReg) (rs2 : GpReg) (a : AddrWord) :
    (executeSub q GpReg.r0 rs1 rs2).memory a = q.memory a :=
  sorry


-- A CLR zeroes the FLAGS special register
theorem clr_zeroes_flags (q : State) :
    (executeWsr q GpReg.r0 GpReg.r0 CsrReg.flags).special CsrReg.flags = 0x0000#16 :=
  sorry


-- A CLR leaves GREATER and GREATER_EQUAL taken, because zeroed flags read as a positive non-zero
-- result, so a CLR followed by a branch on GREATER is an unconditional branch
theorem clr_leaves_greater_taken (q : State) :
    let q' := executeWsr q GpReg.r0 GpReg.r0 CsrReg.flags
    q'.check .greater = true
      /\ q'.check .greaterEqual = true
      /\ q'.check .equal = false
      /\ q'.check .less = false :=
  sorry


-- A SET leaves FLAGS unchanged
theorem set_preserves_flags (q : State) (rd : GpReg) (imm : DataWord) :
    (executeSet q rd imm).special CsrReg.flags = q.special CsrReg.flags :=
  sorry


-- The two SETs that an immediate branch macro expands to leave FLAGS and every comparison
-- unchanged, so the flags set by a preceding CMP still reach the branch
theorem jmpi_preserves_flags (q : State) (hi : DataWord) (lo : DataWord) :
    let q1 := executeSet q GpReg.rscratch0 hi
    let q2 := executeSet q1 GpReg.rscratch1 lo
    q2.special CsrReg.flags = q.special CsrReg.flags
      /\ (forall f : CmpFlag, q2.check f = q.check f) :=
  sorry


-- A GOTO lands the program counter on its immediate address
theorem goto_reaches_target (q : State) (hwf : q.WF) (hi : DataWord) (lo : DataWord)
    (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeSet q GpReg.rscratch0 hi
    let q2 := executeSet q1 GpReg.rscratch1 lo
    let q3 := executeJmp q2 GpReg.rscratch0 GpReg.rscratch1 CmpFlag.always
    q3.pc = (hi ++ lo) :=
  sorry


-- A taken JAL sets RADDR to the address of the next instruction and branches to its target
theorem jal_taken_sets_raddr (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = true) :
    (executeJal q rs1 rs2 cmp).special CsrReg.raddr = q.pc + 2
      /\ (executeJal q rs1 rs2 cmp).pc = (q.register rs1 ++ q.register rs2) :=
  sorry


-- An untaken JAL leaves RADDR unchanged
theorem jal_untaken_preserves_raddr (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = false) :
    (executeJal q rs1 rs2 cmp).special CsrReg.raddr = q.special CsrReg.raddr :=
  sorry


-- A RET branches to the address held in RADDR
theorem ret_returns_to_raddr (q : State) (hwf : q.WF) :
    (executeJmpCsr q CsrReg.raddr CmpFlag.always).pc = q.special CsrReg.raddr :=
  sorry


-- A RET after an unconditional JAL returns to the instruction after the JAL
theorem call_ret_roundtrip (q : State) (hwf : q.WF) (rs1 : GpReg) (rs2 : GpReg)
    (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeJal q rs1 rs2 CmpFlag.always
    let q2 := executeJmpCsr q1 CsrReg.raddr CmpFlag.always
    q2.pc = q.pc + 2 :=
  sorry


-- A PUSH writes its source register at the current stack address and then decrements SADDR
theorem push_decrements_saddr (q : State) (rs : GpReg) (h : 0 < (q.special CsrReg.saddr).toNat) :
    let q' := executeSt q rs GpReg.rscratch1 (-1)
    q'.memory (q.special CsrReg.saddr) = q.register rs
      /\ q'.special CsrReg.saddr = q.special CsrReg.saddr - 1 :=
  sorry


-- A POP increments SADDR and then reads into its destination register from the new stack address
theorem pop_increments_saddr (q : State) (rd : GpReg) (h : Ne rd GpReg.r0)
    (hs : (q.special CsrReg.saddr).toNat < 0xFFFF) :
    let q' := executeLd q rd GpReg.rscratch1 1
    q'.special CsrReg.saddr = q.special CsrReg.saddr + 1
      /\ q'.register rd = q.memory (q.special CsrReg.saddr + 1) :=
  sorry


-- A POP after a PUSH recovers the pushed value and restores SADDR
theorem push_pop_roundtrip (q : State) (rs : GpReg) (rd : GpReg)
    (hrd : Ne rd GpReg.r0)
    (hs : 0 < (q.special CsrReg.saddr).toNat)
    (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeSt q rs GpReg.rscratch1 (-1)
    let q2 := executeLd q1 rd GpReg.rscratch1 1
    q2.register rd = q.register rs
      /\ q2.special CsrReg.saddr = q.special CsrReg.saddr :=
  sorry


-- A POP2 after a PUSH2 recovers both pushed bytes in the right order and restores SADDR, which is
-- the ordering the two macros have to agree on
theorem push2_pop2_roundtrip (q : State) (rs1 : GpReg) (rs2 : GpReg) (rd1 : GpReg) (rd2 : GpReg)
    (hrd1 : Ne rd1 GpReg.r0) (hrd2 : Ne rd2 GpReg.r0) (hrd : Ne rd1 rd2)
    (hs : 1 < (q.special CsrReg.saddr).toNat)
    (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeSt q rs2 GpReg.rscratch1 (-1)
    let q2 := executeSt q1 rs1 GpReg.rscratch1 (-1)
    let q3 := executeLd q2 rd1 GpReg.rscratch1 1
    let q4 := executeLd q3 rd2 GpReg.rscratch1 1
    q4.register rd1 = q.register rs1
      /\ q4.register rd2 = q.register rs2
      /\ q4.special CsrReg.saddr = q.special CsrReg.saddr :=
  sorry


-- A RSR reads the high byte of its special register into RS1 and the low byte into RS2
theorem rsr_splits_csr (q : State) (rd1 : GpReg) (rd2 : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (hrd1 : Ne rd1 GpReg.r0) (hrd2 : Ne rd2 GpReg.r0)
    (hrd : Ne rd1 rd2) :
    let q' := executeRsr q rd1 rd2 csr
    q'.register rd1 = (q.special csr).extractLsb' 8 8
      /\ q'.register rd2 = (q.special csr).extractLsb' 0 8 :=
  sorry


-- A RSR naming the same register twice leaves the low byte in it, following the rule that the
-- later statement of a meaning block wins
theorem rsr_aliased_dest (q : State) (rd : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (hrd : Ne rd GpReg.r0) :
    (executeRsr q rd rd csr).register rd = (q.special csr).extractLsb' 0 8 :=
  sorry


-- A WSR writes RS1 into the high byte and RS2 into the low byte of its special register, masked by
-- that register's write mask
theorem wsr_joins_registers (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (he : Ne csr CsrReg.extns) :
    (executeWsr q rs1 rs2 csr).special csr
      = (q.register rs1 ++ q.register rs2) &&& csr.writeMask :=
  sorry


-- A RSR after a WSR on the same special register recovers the written value masked by that
-- register's write mask, which fixes the operand order of both instructions
theorem wsr_rsr_roundtrip (q : State) (rs1 : GpReg) (rs2 : GpReg) (rd1 : GpReg) (rd2 : GpReg)
    (csr : CsrReg)
    (hu : q.userMode = false) (he : Ne csr CsrReg.extns)
    (hrd1 : Ne rd1 GpReg.r0) (hrd2 : Ne rd2 GpReg.r0) (hrd : Ne rd1 rd2)
    (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeWsr q rs1 rs2 csr
    let q2 := executeRsr q1 rd1 rd2 csr
    (q2.register rd1 ++ q2.register rd2)
      = (q.register rs1 ++ q.register rs2) &&& csr.writeMask :=
  sorry


end MisaSpec

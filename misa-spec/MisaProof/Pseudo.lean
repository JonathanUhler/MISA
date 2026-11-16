import MisaProof.Fault


-- Destination register binders are named rd after the architecture's operand names, which collides
-- with the GpReg.rd constructor
set_option linter.constructorNameAsVariable false


namespace MisaSpec


-- A NOP only advances the program counter
theorem nop_is_pc_increment (q : State) (imm : DataWord) :
    executeSet q GpReg.r0 imm = State.next q :=
  rfl


-- A MOV copies its source register into its destination register
theorem mov_result (q : State) (hwf : q.WF) (rd : GpReg) (rs : GpReg) (h : Ne rd GpReg.r0) :
    (executeOr q rd rs GpReg.r0).register rd = q.register rs := by
  simp only [executeOr, State.next_register, State.writeFlags]
  rw [State.writeReg_self q rd _ h, hwf.zeroReg]
  bv_decide


-- A MOV updates FLAGS, unlike a NOP, so it cannot be placed between a comparison and its branch
theorem mov_clobbers_flags (q : State) (hwf : q.WF) (rd : GpReg) (rs : GpReg) :
    (executeOr q rd rs GpReg.r0).check .equal = (q.register rs == 0x00#8) := by
  simp only [executeOr, State.next_check, State.writeFlags, CsrFlags.eval]
  rw [hwf.zeroReg]
  bv_decide


-- A CMP leaves every register unchanged
theorem cmp_preserves_registers (q : State) (rs1 : GpReg) (rs2 : GpReg) (r : GpReg) :
    (executeSub q GpReg.r0 rs1 rs2).register r = q.register r := by
  simp [executeSub, State.next_register, State.writeFlags, State.writeReg_r0]


-- A CMP leaves memory unchanged
theorem cmp_preserves_memory (q : State) (rs1 : GpReg) (rs2 : GpReg) (a : AddrWord) :
    (executeSub q GpReg.r0 rs1 rs2).memory a = q.memory a := by
  simp [executeSub, next_memory, State.writeFlags, State.writeReg_r0]


-- A CLR zeroes the FLAGS special register
theorem clr_zeroes_flags (q : State) (hwf : q.WF) :
    (executeWsr q GpReg.r0 GpReg.r0 CsrReg.flags).special CsrReg.flags = 0x0000#16 := by
  simp only [executeWsr]
  split
  next hg => simp [CsrReg.userWritable] at hg
  next =>
    rw [State.next_flags, State.writeSpecial_self q CsrReg.flags _ (by decide), hwf.zeroReg]
    simp [CsrReg.writeMask] <;> bv_decide


-- A CLR leaves GREATER_EQUAL and BELOW taken, because zeroed flags read as a non-negative result
-- which also required a borrow, so a CLR followed by a branch on either is an unconditional
-- branch. The two are not contradictory: zeroed flags describe no actual subtraction
theorem clr_leaves_greaterEqual_and_below_taken (q : State) (hwf : q.WF) :
    let q' := executeWsr q GpReg.r0 GpReg.r0 CsrReg.flags
    q'.check .greaterEqual = true
      /\ q'.check .below = true
      /\ q'.check .equal = false
      /\ q'.check .less = false
      /\ q'.check .aboveEqual = false := by
  have hv : (q.register GpReg.r0 ++ q.register GpReg.r0) = 0x0000#16 := by
    rw [hwf.zeroReg]
    bv_decide
  simp only [executeWsr]
  split
  next hg => simp [CsrReg.userWritable] at hg
  next =>
    refine And.intro ?_ (And.intro ?_ (And.intro ?_ (And.intro ?_ ?_)))
    all_goals
      rw [State.next_check, State.writeSpecial_flags_sync, hv]
      simp [CsrReg.writeMask, CsrFlags.dec, CsrFlags.eval] <;> decide


-- A SET leaves FLAGS unchanged
theorem set_preserves_flags (q : State) (rd : GpReg) (imm : DataWord) :
    (executeSet q rd imm).special CsrReg.flags = q.special CsrReg.flags := by
  simp [executeSet, State.next_flags, State.writeReg_special]


-- The two SETs that an immediate branch macro expands to leave FLAGS and every comparison
-- unchanged, so the flags set by a preceding CMP still reach the branch
theorem jmpi_preserves_flags (q : State) (hi : DataWord) (lo : DataWord) :
    let q1 := executeSet q GpReg.rscratch0 hi
    let q2 := executeSet q1 GpReg.rscratch1 lo
    q2.special CsrReg.flags = q.special CsrReg.flags
      /\ (forall f : CmpFlag, q2.check f = q.check f) := by
  refine And.intro ?_ ?_
  next => simp [executeSet, State.next_flags, State.writeReg_special]
  next =>
    intro f
    simp [executeSet, State.next_check, State.writeReg_check]


-- A GOTO lands the program counter on its immediate address
theorem goto_reaches_target (q : State) (hwf : q.WF) (hi : DataWord) (lo : DataWord) :
    let q1 := executeSet q GpReg.rscratch0 hi
    let q2 := executeSet q1 GpReg.rscratch1 lo
    let q3 := executeJmp q2 GpReg.rscratch0 GpReg.rscratch1 CmpFlag.always
    q3.pc = (hi ++ lo) := by
  have hca : q.check CmpFlag.always = true := State.WF.check_always q hwf
  have h1 : (executeSet (executeSet q GpReg.rscratch0 hi) GpReg.rscratch1 lo).register
      GpReg.rscratch0 = hi := by
    simp only [executeSet, State.next_register]
    rw [State.writeReg_other _ GpReg.rscratch1 GpReg.rscratch0 lo (by decide),
        State.next_register]
    exact State.writeReg_self q GpReg.rscratch0 hi (by decide)
  have h2 : (executeSet (executeSet q GpReg.rscratch0 hi) GpReg.rscratch1 lo).register
      GpReg.rscratch1 = lo := by
    simp only [executeSet, State.next_register]
    exact State.writeReg_self _ GpReg.rscratch1 lo (by decide)
  have h3 : (executeSet (executeSet q GpReg.rscratch0 hi) GpReg.rscratch1 lo).check
      CmpFlag.always = true := by
    simp only [executeSet, State.next_check, State.writeReg_check]
    exact hca
  simp [executeJmp, h3, h1, h2]


-- Advancing the program counter leaves RADDR unchanged
theorem next_raddr (q : State) :
    (State.next q).special CsrReg.raddr = q.special CsrReg.raddr := by
  simp only [State.next]
  split
  next => exact (fault_frame q CsrCause.pcOverflow).2.2.2.2.1
  next => rfl


-- A taken JAL sets RADDR to the address of the next instruction and branches to its target
theorem jal_taken_sets_raddr (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = true) :
    (executeJal q rs1 rs2 cmp).special CsrReg.raddr = q.pc + 2
      /\ (executeJal q rs1 rs2 cmp).pc = (q.register rs1 ++ q.register rs2) := by
  refine And.intro ?_ ?_
  next => simp [executeJal, h, State.writeSpecial, CsrReg.writeMask] <;> bv_decide
  next => simp [executeJal, h]


-- An untaken JAL leaves RADDR unchanged
theorem jal_untaken_preserves_raddr (q : State) (rs1 : GpReg) (rs2 : GpReg) (cmp : CmpFlag)
    (h : q.check cmp = false) :
    (executeJal q rs1 rs2 cmp).special CsrReg.raddr = q.special CsrReg.raddr := by
  simp only [executeJal, h]
  exact next_raddr q


-- A RET branches to the address held in RADDR
theorem ret_returns_to_raddr (q : State) (hwf : q.WF) :
    (executeJmpCsr q CsrReg.raddr CmpFlag.always).pc = q.special CsrReg.raddr := by
  simp [executeJmpCsr, CsrReg.userReadable, State.WF.check_always q hwf]


-- A RET after an unconditional JAL returns to the instruction after the JAL
theorem call_ret_roundtrip (q : State) (hwf : q.WF) (rs1 : GpReg) (rs2 : GpReg) :
    let q1 := executeJal q rs1 rs2 CmpFlag.always
    let q2 := executeJmpCsr q1 CsrReg.raddr CmpFlag.always
    q2.pc = q.pc + 2 := by
  have hca : q.check CmpFlag.always = true := State.WF.check_always q hwf
  have h2 := (jal_taken_sets_raddr q rs1 rs2 CmpFlag.always hca).1
  have h1 : (executeJal q rs1 rs2 CmpFlag.always).check CmpFlag.always = true := by
    simp [executeJal, hca, State.writeSpecial]
  simp [executeJmpCsr, CsrReg.userReadable, h1, h2]


-- A PUSH onto a stack outside the reset vector table writes its source register at the current
-- stack address and then decrements SADDR
theorem push_decrements_saddr (q : State) (rs : GpReg) (h : 0 < (q.special CsrReg.saddr).toNat)
    (hv : inVectorTable (q.special CsrReg.saddr) = false) :
    let q' := executeSt q rs GpReg.rscratch1 (-1)
    q'.memory (q.special CsrReg.saddr) = q.register rs
      /\ q'.special CsrReg.saddr = q.special CsrReg.saddr - 1 := by
  have hne : Ne (q.special CsrReg.saddr) 0x0000#16 := by
    intro he
    rw [he] at h
    simp at h
  have heff : effAddr (q.special CsrReg.saddr) (-1) = some (q.special CsrReg.saddr - 1) := by
    have hc : ((q.special CsrReg.saddr).setWidth 17 + (-1 : HalfWord).signExtend 17).getLsbD 16
        = false := by bv_decide
    simp [effAddr] <;> bv_decide
  exact st_stack_order q rs (-1) (q.special CsrReg.saddr - 1) hv heff


-- A POP increments SADDR and then reads into its destination register from the new stack address
theorem pop_increments_saddr (q : State) (rd : GpReg) (h : Ne rd GpReg.r0)
    (hs : (q.special CsrReg.saddr).toNat < 0xFFFF) :
    let q' := executeLd q rd GpReg.rscratch1 1
    q'.special CsrReg.saddr = q.special CsrReg.saddr + 1
      /\ q'.register rd = q.memory (q.special CsrReg.saddr + 1) := by
  have hne : Ne (q.special CsrReg.saddr) 0xFFFF#16 := by
    intro he
    rw [he] at hs
    simp at hs
  have heff : effAddr (q.special CsrReg.saddr) 1 = some (q.special CsrReg.saddr + 1) := by
    have hc : ((q.special CsrReg.saddr).setWidth 17 + (1 : HalfWord).signExtend 17).getLsbD 16
        = false := by bv_decide
    simp [effAddr] <;> bv_decide
  exact ld_stack_order q rd 1 (q.special CsrReg.saddr + 1) h heff


-- A POP after a PUSH recovers the pushed value and restores SADDR
theorem push_pop_roundtrip (q : State) (rs : GpReg) (rd : GpReg)
    (hrd : Ne rd GpReg.r0)
    (hs : 0 < (q.special CsrReg.saddr).toNat)
    (hv : inVectorTable (q.special CsrReg.saddr) = false) :
    let q1 := executeSt q rs GpReg.rscratch1 (-1)
    let q2 := executeLd q1 rd GpReg.rscratch1 1
    q2.register rd = q.register rs
      /\ q2.special CsrReg.saddr = q.special CsrReg.saddr := by
  have hne : Ne (q.special CsrReg.saddr) 0x0000#16 := by
    intro he
    rw [he] at hs
    simp at hs
  have hp := push_decrements_saddr q rs hs hv
  have heff : effAddr ((executeSt q rs GpReg.rscratch1 (-1)).special CsrReg.saddr) 1
      = some (q.special CsrReg.saddr) := by
    rw [hp.2]
    have hc : ((q.special CsrReg.saddr - 1).setWidth 17 + (1 : HalfWord).signExtend 17).getLsbD 16
        = false := by bv_decide
    simp [effAddr] <;> bv_decide
  have hl := ld_stack_order (executeSt q rs GpReg.rscratch1 (-1)) rd 1
      (q.special CsrReg.saddr) hrd heff
  refine And.intro ?_ hl.1
  rw [hl.2]
  exact hp.1


-- A POP2 after a PUSH2 recovers both pushed bytes in the right order and restores SADDR, which is
-- the ordering the two macros have to agree on
theorem push2_pop2_roundtrip (q : State) (rs1 : GpReg) (rs2 : GpReg) (rd1 : GpReg) (rd2 : GpReg)
    (hrd1 : Ne rd1 GpReg.r0) (hrd2 : Ne rd2 GpReg.r0) (hrd : Ne rd1 rd2)
    (hs : 1 < (q.special CsrReg.saddr).toNat)
    (hv : inVectorTable (q.special CsrReg.saddr) = false) :
    let q1 := executeSt q rs2 GpReg.rscratch1 (-1)
    let q2 := executeSt q1 rs1 GpReg.rscratch1 (-1)
    let q3 := executeLd q2 rd1 GpReg.rscratch1 1
    let q4 := executeLd q3 rd2 GpReg.rscratch1 1
    q4.register rd1 = q.register rs1
      /\ q4.register rd2 = q.register rs2
      /\ q4.special CsrReg.saddr = q.special CsrReg.saddr := by
  have hS0 : Ne (q.special CsrReg.saddr) 0x0000#16 := by
    intro he
    rw [he] at hs
    simp at hs
  have hS1 : Ne (q.special CsrReg.saddr) 0x0001#16 := by
    intro he
    rw [he] at hs
    simp at hs
  have h0 : 0 < (q.special CsrReg.saddr).toNat := by omega
  -- the byte below the stack address is outside the vector table too, so neither push faults
  have hv2 : inVectorTable (q.special CsrReg.saddr - 1) = false := by
    rw [inVectorTable_eq_false_iff] at hv
    rw [inVectorTable_eq_false_iff]
    bv_decide
  -- the first push stores RS2 at the stack address and steps SADDR down
  have p1 := push_decrements_saddr q rs2 h0 hv
  -- the second push stores RS1 one below it
  have heff2 : effAddr (q.special CsrReg.saddr - 1) (-1)
      = some (q.special CsrReg.saddr - 1 - 1) := by
    have hc : ((q.special CsrReg.saddr - 1).setWidth 17
        + (-1 : HalfWord).signExtend 17).getLsbD 16 = false := by bv_decide
    simp [effAddr] <;> bv_decide
  have p2 := st_stack_order (executeSt q rs2 GpReg.rscratch1 (-1)) rs1 (-1)
      (q.special CsrReg.saddr - 1 - 1) (by rw [p1.2]; exact hv2) (by rw [p1.2]; exact heff2)
  -- the first pop reads RS1 back
  have heff3 : effAddr (q.special CsrReg.saddr - 1 - 1) 1
      = some (q.special CsrReg.saddr - 1) := by
    have hc : ((q.special CsrReg.saddr - 1 - 1).setWidth 17
        + (1 : HalfWord).signExtend 17).getLsbD 16 = false := by bv_decide
    simp [effAddr] <;> bv_decide
  have p3 := ld_stack_order (executeSt (executeSt q rs2 GpReg.rscratch1 (-1)) rs1
      GpReg.rscratch1 (-1)) rd1 1 (q.special CsrReg.saddr - 1) hrd1 (by rw [p2.2]; exact heff3)
  -- the second pop reads RS2 back and restores SADDR
  have heff4 : effAddr (q.special CsrReg.saddr - 1) 1 = some (q.special CsrReg.saddr) := by
    have hc : ((q.special CsrReg.saddr - 1).setWidth 17
        + (1 : HalfWord).signExtend 17).getLsbD 16 = false := by bv_decide
    simp [effAddr] <;> bv_decide
  have p4 := ld_stack_order (executeLd (executeSt (executeSt q rs2 GpReg.rscratch1 (-1)) rs1
      GpReg.rscratch1 (-1)) rd1 GpReg.rscratch1 1) rd2 1 (q.special CsrReg.saddr) hrd2
      (by rw [p3.1]; exact heff4)
  have hne : Ne (q.special CsrReg.saddr)
      ((executeSt q rs2 GpReg.rscratch1 (-1)).special CsrReg.saddr) := by
    rw [p1.2]
    bv_decide
  refine And.intro ?_ (And.intro ?_ p4.1)
  next =>
    rw [ld_preserves_registers_other _ rd2 GpReg.rscratch1 1 rd1 hrd, p3.2, <- p1.2, p2.1,
        st_preserves_registers q rs2 GpReg.rscratch1 (-1) rs1]
  next =>
    rw [p4.2, ld_preserves_memory, st_preserves_memory_other _ rs1 (-1) _ hne, p1.1]


-- A RSR reads the high byte of its special register into RS1 and the low byte into RS2
theorem rsr_splits_csr (q : State) (rd1 : GpReg) (rd2 : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (hrd1 : Ne rd1 GpReg.r0) (hrd2 : Ne rd2 GpReg.r0)
    (hrd : Ne rd1 rd2) :
    let q' := executeRsr q rd1 rd2 csr
    q'.register rd1 = (q.special csr).extractLsb' 8 8
      /\ q'.register rd2 = (q.special csr).extractLsb' 0 8 := by
  simp only [executeRsr]
  split
  next hg =>
    rw [hu] at hg
    simp at hg
  next =>
    refine And.intro ?_ ?_
    next =>
      rw [State.next_register, State.writeReg_other _ rd2 rd1 _ hrd]
      exact State.writeReg_self q rd1 _ hrd1
    next =>
      rw [State.next_register]
      exact State.writeReg_self _ rd2 _ hrd2


-- A RSR naming the same register twice leaves the low byte in it, following the rule that the
-- later statement of a meaning block wins
theorem rsr_aliased_dest (q : State) (rd : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (hrd : Ne rd GpReg.r0) :
    (executeRsr q rd rd csr).register rd = (q.special csr).extractLsb' 0 8 := by
  simp only [executeRsr]
  split
  next hg =>
    rw [hu] at hg
    simp at hg
  next =>
    rw [State.next_register]
    exact State.writeReg_self _ rd _ hrd


-- A WSR writes RS1 into the high byte and RS2 into the low byte of its special register, masked by
-- that register's write mask. The program counter bound is needed because a PC overflow would
-- record CAUSE, PRIVS, and FAULT over the write.
theorem wsr_joins_registers (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (he : Ne csr CsrReg.extns) (hpc : q.pc < 0xFFFE#16) :
    (executeWsr q rs1 rs2 csr).special csr
      = (q.register rs1 ++ q.register rs2) &&& csr.writeMask := by
  simp only [executeWsr]
  split
  next hg =>
    rw [hu] at hg
    simp at hg
  next =>
    have hpw : (q.writeSpecial csr (q.register rs1 ++ q.register rs2)).pc < 0xFFFE#16 := by
      rw [State.writeSpecial_pc]
      exact hpc
    rw [next_special_of_lt _ csr hpw]
    exact State.writeSpecial_self q csr _ he


-- A WSR naming a register other than EXTNS and PRIVS cannot leave Machine Mode, since neither the
-- written register nor the increment can reach PRIVS
theorem wsr_preserves_userMode (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg)
    (hu : q.userMode = false) (he : Ne csr CsrReg.extns) (hp : Ne csr CsrReg.privs)
    (hpc : q.pc < 0xFFFE#16) :
    (executeWsr q rs1 rs2 csr).userMode = false := by
  simp only [executeWsr]
  split
  next hg =>
    rw [hu] at hg
    simp at hg
  next =>
    have hpw : (q.writeSpecial csr (q.register rs1 ++ q.register rs2)).pc < 0xFFFE#16 := by
      rw [State.writeSpecial_pc]
      exact hpc
    simp only [State.userMode]
    rw [next_special_of_lt _ CsrReg.extns hpw, next_special_of_lt _ CsrReg.privs hpw,
        State.writeSpecial_other q csr CsrReg.extns _ (Ne.symm he),
        State.writeSpecial_other q csr CsrReg.privs _ (Ne.symm hp)]
    exact hu


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
      = (q.register rs1 ++ q.register rs2) &&& csr.writeMask := by
  have hpc' : q.pc < 0xFFFE#16 := by bv_decide
  have h1 : (executeWsr q rs1 rs2 csr).special csr
      = (q.register rs1 ++ q.register rs2) &&& csr.writeMask :=
    wsr_joins_registers q rs1 rs2 csr hu he hpc'
  -- the read back cannot fault: either the register is readable from User Mode, or the write
  -- cannot have left Machine Mode
  have hguard : ((executeWsr q rs1 rs2 csr).userMode && !csr.userReadable) = false := by
    cases csr
    case extns => exact absurd rfl he
    case saddr => simp [CsrReg.userReadable]
    case raddr => simp [CsrReg.userReadable]
    case flags => simp [CsrReg.userReadable]
    case privs => simp [CsrReg.userReadable]
    case cause => rw [wsr_preserves_userMode q rs1 rs2 CsrReg.cause hu he (by decide) hpc']; rfl
    case fault => rw [wsr_preserves_userMode q rs1 rs2 CsrReg.fault hu he (by decide) hpc']; rfl
    case retsc => rw [wsr_preserves_userMode q rs1 rs2 CsrReg.retsc hu he (by decide) hpc']; rfl
    case retir => rw [wsr_preserves_userMode q rs1 rs2 CsrReg.retir hu he (by decide) hpc']; rfl
  simp only [executeRsr]
  split
  next hg =>
    rw [hguard] at hg
    simp at hg
  next =>
    simp only [State.next_register]
    rw [State.writeReg_other _ rd2 rd1 _ hrd, State.writeReg_self _ rd1 _ hrd1,
        State.writeReg_self _ rd2 _ hrd2, h1]
    bv_decide


end MisaSpec

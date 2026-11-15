import MisaProof.Fault


namespace MisaSpec


-- Taking a fault leaves the processor in Machine Mode
theorem fault_enters_machine_mode (q : State) (c : CsrCause) : (q.fault c).userMode = false :=
  sorry


-- A successful SYSCALL leaves the processor in Machine Mode
theorem syscall_enters_machine_mode (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) :
    (executeSyscall q rs).userMode = false :=
  sorry


-- Taking an interrupt leaves the processor in Machine Mode
theorem interrupt_enters_machine_mode (q : State) : q.interrupt.userMode = false :=
  sorry


-- A step taken in User Mode either stays in User Mode or ends up in one of the three trap
-- handlers, so User Mode cannot raise its own privilege
theorem user_cannot_escalate (q : State) (hwf : q.WF) (hh : q.halted = false)
    (hu : q.userMode = true) :
    (step q).userMode = true
      \/ (step q).pc = q.faultVector
      \/ (step q).pc = q.syscallVector
      \/ (step q).pc = q.intVector :=
  sorry


-- The special registers readable from User Mode are SADDR, RADDR, FLAGS, and PRIVS
theorem userReadable_table (csr : CsrReg) :
    csr.userReadable
      = (decide (csr = CsrReg.saddr) || decide (csr = CsrReg.raddr)
         || decide (csr = CsrReg.flags) || decide (csr = CsrReg.privs)) :=
  sorry


-- The special registers writable from User Mode are SADDR, RADDR, and FLAGS
theorem userWritable_table (csr : CsrReg) :
    csr.userWritable
      = (decide (csr = CsrReg.saddr) || decide (csr = CsrReg.raddr)
         || decide (csr = CsrReg.flags)) :=
  sorry


-- Every special register writable from User Mode is also readable from User Mode
theorem userWritable_subset_userReadable (csr : CsrReg) (h : csr.userWritable = true) :
    csr.userReadable = true :=
  sorry


-- PRIVS is readable but not writable from User Mode
theorem privs_readable_not_writable :
    CsrReg.privs.userReadable = true /\ CsrReg.privs.userWritable = false :=
  sorry


-- EXTNS is not writable from User Mode, so User Mode cannot disable the Privilege Extension to
-- escape it
theorem extns_not_userWritable : CsrReg.extns.userWritable = false :=
  sorry


-- A JMP to RETSC from User Mode raises a privileged instruction fault, which takes precedence over
-- both of the control event faults that target could otherwise raise
theorem jmpCsr_privilege_precedence_retsc (q : State) (cmp : CmpFlag) (hu : q.userMode = true) :
    executeJmpCsr q CsrReg.retsc cmp = q.fault CsrCause.privilegedInstruction :=
  sorry


-- A JMP to RETIR from User Mode raises a privileged instruction fault, which takes precedence over
-- both of the control event faults that target could otherwise raise
theorem jmpCsr_privilege_precedence_retir (q : State) (cmp : CmpFlag) (hu : q.userMode = true) :
    executeJmpCsr q CsrReg.retir cmp = q.fault CsrCause.privilegedInstruction :=
  sorry


-- A JMP to RETSC or RETIR with a comparison other than ALWAYS raises a conditional context return
-- fault whether or not the comparison evaluates true, because the fault is a property of the
-- instruction encoding
theorem conditional_context_return_unconditional (q : State) (csr : CsrReg) (cmp : CmpFlag)
    (hu : q.userMode = false) (hc : Ne cmp CmpFlag.always)
    (h : csr = CsrReg.retsc \/ csr = CsrReg.retir) :
    executeJmpCsr q csr cmp = q.fault (CsrCause.conditionalContextReturn cmp csr) :=
  sorry


-- A HALT in User Mode raises a privileged instruction fault
theorem halt_privileged_in_user_mode (q : State) (rs : GpReg) (hu : q.userMode = true) :
    executeHalt q rs = q.fault CsrCause.privilegedInstruction :=
  sorry


-- A SYSCALL in User Mode is not privileged, since it is how User Mode reaches Machine Mode
theorem syscall_allowed_in_user_mode (q : State) (rs : GpReg) (hu : q.userMode = true)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) :
    Ne (executeSyscall q rs) (q.fault CsrCause.privilegedInstruction) :=
  sorry


-- No WSR can reach the internal slots holding the automatically saved RSCRATCH, FLAGS, and PRIVS
-- values, which have no special register encoding and so cannot be read either
theorem wsr_cannot_reach_int_slots (q : State) (rs1 : GpReg) (rs2 : GpReg) (csr : CsrReg) :
    let q' := executeWsr q rs1 rs2 csr
    q'.intRscratch = q.intRscratch
      /\ q'.intFlags = q.intFlags
      /\ q'.intPrivs = q.intPrivs
      /\ q'.syscallPrivs = q.syscallPrivs :=
  sorry


-- Only taking an interrupt writes the interrupt save slots, so an interrupt handler cannot forge
-- the context it will return to
theorem step_preserves_int_slots (q : State)
    (h : Not ((q.intPending && q.canInterrupt) = true)) :
    let q' := step q
    q'.intRscratch = q.intRscratch /\ q'.intFlags = q.intFlags /\ q'.intPrivs = q.intPrivs :=
  sorry


-- Taking an interrupt leaves the system call PRIVS slot alone, so an interrupt inside a system call
-- context does not disturb the value the system call return will restore
theorem interrupt_preserves_syscall_privs (q : State) :
    q.interrupt.syscallPrivs = q.syscallPrivs :=
  sorry


-- A SYSCALL saves the current PRIVS value into the system call slot
theorem syscall_saves_privs (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) :
    (executeSyscall q rs).syscallPrivs = q.special CsrReg.privs :=
  sorry


-- Taking an interrupt saves the current PRIVS value into the interrupt slot
theorem interrupt_saves_privs (q : State) : q.interrupt.intPrivs = q.special CsrReg.privs :=
  sorry


-- A WSR to PRIVS inside an interrupt context does not survive the return, because the restore
-- overwrites PRIVS entirely
theorem wsr_privs_does_not_survive_return (q : State) (hwf : q.WF) (rs1 : GpReg) (rs2 : GpReg)
    (hu : q.userMode = false) (hi : q.inInterrupt = true) (hpc : q.pc < 0xFFF0#16) :
    let q1 := executeWsr q rs1 rs2 CsrReg.privs
    let q2 := executeJmpCsr q1 CsrReg.retir CmpFlag.always
    q2.special CsrReg.privs = q.intPrivs &&& CsrReg.privs.writeMask :=
  sorry


-- Taking an interrupt sets RETIR to the address of the preempted instruction, so the return
-- resumes with that instruction rather than skipping it
theorem interrupt_saves_preempted_pc (q : State) :
    q.interrupt.special CsrReg.retir = q.pc :=
  sorry


-- A SYSCALL sets RETSC to the address after itself, since the SYSCALL is the instruction that
-- entered the context
theorem syscall_saves_next_pc (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) :
    (executeSyscall q rs).special CsrReg.retsc = q.pc + 2 :=
  sorry


-- Taking an interrupt continues execution at the interrupt handler address in the reset vector
-- table
theorem interrupt_jumps_to_vector (q : State) : q.interrupt.pc = q.intVector :=
  sorry


-- A SYSCALL continues execution at the syscall handler address in the reset vector table
theorem syscall_jumps_to_vector (q : State) (rs : GpReg)
    (hs : q.inSyscall = false) (hi : q.inInterrupt = false) :
    (executeSyscall q rs).pc = q.syscallVector :=
  sorry


-- Taking an interrupt leaves CAUSE unchanged, unlike a system call or a fault
theorem interrupt_preserves_cause (q : State) :
    q.interrupt.special CsrReg.cause = q.special CsrReg.cause :=
  sorry


-- An interrupt that returns immediately restores RSCRATCH0, RSCRATCH1, FLAGS, PRIVS, and the
-- program counter, so an empty interrupt handler is transparent
theorem interrupt_transparent (q : State) (hwf : q.WF) :
    let q' := executeJmpCsr q.interrupt CsrReg.retir CmpFlag.always
    q'.register GpReg.rscratch0 = q.register GpReg.rscratch0
      /\ q'.register GpReg.rscratch1 = q.register GpReg.rscratch1
      /\ q'.special CsrReg.flags = q.special CsrReg.flags
      /\ q'.special CsrReg.privs = q.special CsrReg.privs
      /\ q'.pc = q.pc
      /\ q'.inInterrupt = false :=
  sorry


-- A SYSCALL in a system call or interrupt context raises an improper system call fault
theorem no_nested_syscall (q : State) (rs : GpReg)
    (h : q.inSyscall = true \/ q.inInterrupt = true) :
    executeSyscall q rs = q.fault (CsrCause.improperSystemCall q.inSyscall q.inInterrupt) :=
  sorry


-- An interrupt cannot be taken from an interrupt context
theorem no_nested_interrupt (q : State) (h : q.inInterrupt = true) : q.canInterrupt = false :=
  sorry


-- A JMP to RETSC outside a system call context, or inside an interrupt context, raises an improper
-- return context fault
theorem retsc_improper_context (q : State) (hu : q.userMode = false)
    (h : q.inSyscall = false \/ q.inInterrupt = true) :
    executeJmpCsr q CsrReg.retsc CmpFlag.always
      = q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt CsrReg.retsc) :=
  sorry


-- A JMP to RETIR outside an interrupt context raises an improper return context fault
theorem retir_improper_context (q : State) (hu : q.userMode = false) (h : q.inInterrupt = false) :
    executeJmpCsr q CsrReg.retir CmpFlag.always
      = q.fault (CsrCause.improperReturnContext q.inSyscall q.inInterrupt CsrReg.retir) :=
  sorry


-- A JMP to RETSC from a system call context leaves the context, branches to RETSC, and restores
-- the saved PRIVS value
theorem retsc_leaves_syscall_context (q : State) (hwf : q.WF) (hu : q.userMode = false)
    (hs : q.inSyscall = true) (hi : q.inInterrupt = false) :
    let q' := executeJmpCsr q CsrReg.retsc CmpFlag.always
    q'.inSyscall = false
      /\ q'.pc = q.special CsrReg.retsc
      /\ q'.special CsrReg.privs = q.syscallPrivs &&& CsrReg.privs.writeMask :=
  sorry


-- Taking an interrupt does not change whether the processor is in a system call context
theorem interrupt_preserves_syscall_context (q : State) :
    q.interrupt.inSyscall = q.inSyscall :=
  sorry


-- Stepping a halted processor leaves it unchanged, so HALT is terminal and a halted processor does
-- not take interrupts
theorem halted_is_fixpoint (q : State) (h : q.halted = true) : step q = q :=
  sorry


-- A HALT halts the processor and records the halt cause with the RS register as extended status
theorem halt_sets_halted (q : State) (rs : GpReg) (hu : q.userMode = false) :
    (executeHalt q rs).halted = true
      /\ (executeHalt q rs).special CsrReg.cause
           = (CsrCause.haltInstruction (q.register rs)).enc &&& CsrReg.cause.writeMask :=
  sorry


-- Every step clears the pending interrupt flag, so pin activity that cannot be taken is discarded
-- rather than retained and the source has to reassert the pin
theorem step_clears_int_pending (q : State) (h : q.halted = false) :
    (step q).intPending = false :=
  sorry


-- A pending interrupt is taken before the next instruction is fetched or executed, so an
-- instruction that would have faulted raises its fault only after the interrupt returns
theorem interrupt_preempts_instruction (q : State) (h : q.halted = false)
    (hp : q.intPending = true) (hc : q.canInterrupt = true) :
    step q = q.interrupt :=
  sorry


-- Taking a fault clears PRIVS even when the Privilege Extension is disabled, which contradicts the
-- rule that a disabled extension's registers keep their contents. EXPECTED TO FAIL: needs a
-- specification decision on whether the points of privilege change apply while the extension is
-- disabled, since nothing ever restores the ring bit lost on the fault path.
theorem fault_preserves_privs_when_extension_disabled (q : State)
    (h : (CsrExtns.dec (q.special CsrReg.extns)).privilege = false) (c : CsrCause) :
    (q.fault c).special CsrReg.privs = q.special CsrReg.privs :=
  sorry


end MisaSpec

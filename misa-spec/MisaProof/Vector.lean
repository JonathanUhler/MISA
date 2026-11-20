import MisaProof.Fault


-- Destination register binders are named rd after the architecture's operand names, which collides
-- with the GpReg.rd constructor
set_option linter.constructorNameAsVariable false


/-
The reset vector table is not writable by software.

The architecture requires that nothing a program executes can change the sixteen bytes at
0xFFF0 - 0xFFFF, because a trap loads its handler address from that table while entering Machine
Mode. The theorems below close that off in three layers:

- ST is the only instruction that writes memory, and both of its variants raise a vector write
  fault instead of writing when the address they would write lies in the table. The address checked
  is the one written, which for the stack variant is the current SADDR rather than the updated one.
- Executing any instruction, taking any fault, and entering an interrupt all leave the table alone.
  This is proved over every instruction rather than over the ones that happen to touch memory, so a
  new instruction cannot quietly open a second path.
- Over a run of any length, with the interrupt pin asserted on any set of steps, the table still
  holds the bytes it held when the run started.

The model has exactly one memory mutating primitive, State.writeMem, so "no other path" is a
property of step rather than an assumption about which instructions were considered.
-/


namespace MisaSpec


-- An address at or above the base of the reset vector table is in the table
theorem inVectorTable_of_le (addr : AddrWord) (h : 0xFFF0#16 <= addr) :
    inVectorTable addr = true :=
  decide_eq_true h


-- An address in the reset vector table differs from any address a store is allowed to write
theorem ne_of_inVectorTable (a : AddrWord) (b : AddrWord)
    (ha : inVectorTable a = true) (hb : Not (inVectorTable b = true)) : Ne a b := by
  intro he
  rw [he] at ha
  exact absurd ha hb


-- No store into the reset vector table is ever permitted to proceed. This holds in either
-- privilege level, under any set of extensions, and whatever the memory map provides there: the
-- vector test sits inside the access check, so every path through it yields a fault.
theorem accessFault_vector_isSome (q : State) (a : AddrWord) (hv : inVectorTable a = true) :
    Exists fun cause : CsrCause => q.accessFault AccessKind.write a = some cause := by
  simp only [State.accessFault, State.mapFault]
  split
  next => exact Exists.intro _ rfl
  next =>
    cases hc : q.caps a with
    | none   => exact Exists.intro _ rfl
    | some c => exact Exists.intro (CsrCause.vectorWrite a)
                  (by simp [AccessKind.isVectorWrite, hv])


-- A store that the access check allows is therefore not a store into the reset vector table
theorem accessFault_none_not_vector (q : State) (a : AddrWord)
    (h : q.accessFault AccessKind.write a = none) : Not (inVectorTable a = true) := by
  intro hv
  cases accessFault_vector_isSome q a hv with
  | intro cause hc => rw [hc] at h; exact absurd h (by simp)


-- Where the map provides memory and the MPU permits the write, the fault reported for a store
-- into the table is the vector write. The ordering puts it ahead of an invalid access, which is
-- what an implementation holding its vector table in read only memory would otherwise report.
theorem accessFault_vectorWrite (q : State) (a : AddrWord) (hv : inVectorTable a = true)
    (hmpu : (q.mpuCaps a).allows AccessKind.write = true)
    (hmapped : Exists fun c : MemCaps => q.caps a = some c) :
    q.accessFault AccessKind.write a = some (CsrCause.vectorWrite a) := by
  cases hmapped with
  | intro c hc => simp [State.accessFault, State.mapFault, AccessKind.isVectorWrite, hmpu, hc, hv]


-- A stack ST whose stack address is in the reset vector table raises a vector write fault naming
-- the address it would have written
theorem st_stack_vector_fault (q : State) (rd : GpReg) (imm : HalfWord) (s : AddrWord)
    (hv : inVectorTable (q.special CsrReg.saddr) = true)
    (hmpu : (q.mpuCaps (q.special CsrReg.saddr)).allows AccessKind.write = true)
    (hmapped : Exists fun c : MemCaps => q.caps (q.special CsrReg.saddr) = some c)
    (h : effAddr (q.special CsrReg.saddr) imm = some s) :
    executeSt q rd GpReg.rscratch1 imm
      = q.fault (CsrCause.vectorWrite (q.special CsrReg.saddr)) := by
  simp [executeSt, h, accessFault_vectorWrite q _ hv hmpu hmapped]


-- A register-based ST whose effective address is in the reset vector table raises a vector write
-- fault naming that address
theorem st_base_vector_fault (q : State) (rd : GpReg) (rsb : GpReg) (lo : GpReg) (imm : HalfWord)
    (addr : AddrWord) (hrsb : Ne rsb GpReg.rscratch1)
    (hlo : lo = (GpReg.dec (rsb.enc + 1)).getD GpReg.r0)
    (h : effAddr (q.register rsb ++ q.register lo) imm = some addr)
    (hv : inVectorTable addr = true)
    (hmpu : (q.mpuCaps addr).allows AccessKind.write = true)
    (hmapped : Exists fun c : MemCaps => q.caps addr = some c) :
    executeSt q rd rsb imm = q.fault (CsrCause.vectorWrite addr) := by
  subst hlo
  cases rsb
  case rscratch1 => exact absurd rfl hrsb
  all_goals
    simp only [executeSt, h, accessFault_vectorWrite q _ hv hmpu hmapped]


-- A ST into the reset vector table records a vector write in CAUSE and enters the fault handler
theorem st_vector_cause (q : State) (addr : AddrWord) (hv : q.faultVectorReadable) :
    (q.fault (CsrCause.vectorWrite addr)).special CsrReg.cause
        = (CsrCause.vectorWrite addr).enc &&& CsrReg.cause.writeMask
      /\ (q.fault (CsrCause.vectorWrite addr)).pc = q.faultVector :=
  And.intro (fault_sets_cause q _ hv) (fault_jumps_to_vector q _ hv)


-- A stack ST into the reset vector table leaves SADDR and memory unchanged, so the write is
-- ignored and the stack does not move
theorem st_stack_vector_no_effect (q : State) (rd : GpReg) (imm : HalfWord)
    (hv : inVectorTable (q.special CsrReg.saddr) = true) :
    let q' := executeSt q rd GpReg.rscratch1 imm
    q'.special CsrReg.saddr = q.special CsrReg.saddr
      /\ (forall a : AddrWord, q'.memory a = q.memory a) := by
  simp only [executeSt]
  split
  next => exact And.intro (fault_frame q _).2.2.2.1 (fault_frame q _).2.1
  next =>
    cases accessFault_vector_isSome q (q.special CsrReg.saddr) hv with
    | intro cause hc =>
      simp only [hc]
      exact And.intro (fault_frame q _).2.2.2.1 (fault_frame q _).2.1


-- Every ST leaves the reset vector table unchanged, whichever variant it is and wherever it points
theorem st_preserves_vectors (q : State) (rd : GpReg) (rsb : GpReg) (imm : HalfWord)
    (a : AddrWord) (h : inVectorTable a = true) :
    (executeSt q rd rsb imm).memory a = q.memory a := by
  simp only [executeSt]
  repeat (any_goals split)
  all_goals first
    | exact (fault_frame _ _).2.1 a
    | (simp only [next_memory, State.writeSpecial_memory]
       exact State.writeMem_other _ _ a _
         (ne_of_inVectorTable a _ h (accessFault_none_not_vector _ _ (by assumption))))


-- Executing a decoded instruction leaves the reset vector table unchanged. Every instruction is
-- covered, not only the two that reach memory, so the result does not depend on which instructions
-- were thought to be able to write.
theorem decodeAndExecute_preserves_vectors (q : State) (a : AddrWord)
    (h : inVectorTable a = true) :
    (decodeAndExecute q).memory a = q.memory a := by
  simp only [decodeAndExecute]
  -- a fetch that faults, then a word that fails to decode, then every instruction
  split
  next => exact (fault_frame q _).2.1 a
  next =>
    split
    next => exact (fault_frame q _).2.1 a
    next =>
      split
      all_goals first
        | exact st_preserves_vectors q _ _ _ a h
        | exact ld_preserves_memory q _ _ _ a
        | (simp only [executeHalt, executeSyscall, executeAdd, executeAdc, executeSub, executeSbb,
                      executeAnd, executeOr, executeXor, executeRrc, executeSet, executeRsr,
                      executeWsr, executeJal, executeJmp, executeJmpCsr]
           repeat (any_goals split)
           all_goals first
             | rfl
             | exact (fault_frame _ _).2.1 a
             | exact (faultAt_frame _ _ _).2.1 a
             | simp [next_memory, State.writeReg_memory, State.writeSpecial_memory,
                     State.writeFlags])


-- Fetching and executing leaves the reset vector table unchanged
theorem execute_preserves_vectors (q : State) (a : AddrWord) (h : inVectorTable a = true) :
    (execute q).memory a = q.memory a := by
  simp only [execute]
  split
  next => exact (fault_frame q _).2.1 a
  next => exact decodeAndExecute_preserves_vectors q a h


-- Entering an interrupt leaves memory unchanged, since it only saves state into the shadow slots.
-- So does the fault it takes instead when the interrupt vector cannot be read.
theorem interrupt_preserves_memory (q : State) (a : AddrWord) :
    q.interrupt.memory a = q.memory a := by
  simp only [State.interrupt]
  split
  next => rfl
  next => exact (faultAt_frame _ _ _).2.1 a


-- A step leaves the reset vector table unchanged, whether it executes an instruction, takes a
-- fault, or enters an interrupt
theorem step_preserves_vectors (q : State) (a : AddrWord) (h : inVectorTable a = true) :
    (step q).memory a = q.memory a := by
  simp only [step]
  split
  next => rfl
  next =>
    split
    next => exact interrupt_preserves_memory q a
    next => exact execute_preserves_vectors _ a h


-- A step leaves all four handler addresses as the program found them, so no instruction can
-- retarget the reset, fault, system call, or interrupt vector
theorem step_preserves_vector_table (q : State) :
    (step q).faultVector = q.faultVector
      /\ (step q).syscallVector = q.syscallVector
      /\ (step q).intVector = q.intVector
      /\ (step q).resetVector = q.resetVector := by
  simp only [State.faultVector, State.syscallVector, State.intVector, State.resetVector]
  refine And.intro ?_ (And.intro ?_ (And.intro ?_ ?_))
  all_goals
    rw [step_preserves_vectors q _ (by decide), step_preserves_vectors q _ (by decide)]


-- A run of n steps, where the pin argument supplies the pending interrupt flag ahead of each step.
-- Any interrupt schedule the driver's run loop can produce is one of these pin sequences. The loop
-- stops at a halted state and this does not, which costs nothing: a step of a halted state is a
-- fixpoint, so the extra steps change nothing.
def State.runN (pin : Nat -> Bool) (n : Nat) (q : State) : State :=
  match n with
  | 0      => q
  | n' + 1 => State.runN (fun k => pin (k + 1)) n' (step { q with intPending := pin 0 })


-- The reset vector table holds the same sixteen bytes after a run of any length under any
-- interrupt schedule, which is the guarantee that nothing a program executes can overwrite it
theorem runN_preserves_vectors (pin : Nat -> Bool) (n : Nat) (q : State) (a : AddrWord)
    (h : inVectorTable a = true) :
    (State.runN pin n q).memory a = q.memory a := by
  induction n generalizing pin q with
  | zero       => rfl
  | succ n' ih =>
    simp only [State.runN]
    rw [ih (fun k => pin (k + 1)) (step { q with intPending := pin 0 })]
    exact step_preserves_vectors { q with intPending := pin 0 } a h


-- Every trap taken anywhere in a run therefore lands on the handler address the run started with
theorem runN_preserves_vector_table (pin : Nat -> Bool) (n : Nat) (q : State) :
    (State.runN pin n q).faultVector = q.faultVector
      /\ (State.runN pin n q).syscallVector = q.syscallVector
      /\ (State.runN pin n q).intVector = q.intVector
      /\ (State.runN pin n q).resetVector = q.resetVector := by
  simp only [State.faultVector, State.syscallVector, State.intVector, State.resetVector]
  refine And.intro ?_ (And.intro ?_ (And.intro ?_ ?_))
  all_goals
    rw [runN_preserves_vectors pin n q _ (by decide),
        runN_preserves_vectors pin n q _ (by decide)]


-- Reset reads the vector table out of the program image, so the handler addresses a run can reach
-- are exactly the ones the image was built with
theorem runN_vectors_eq_image (bytes : ByteArray) (extns : SpecWord) (map : MemMap)
    (pin : Nat -> Bool) (n : Nat) (a : AddrWord) (h : inVectorTable a = true) :
    (State.runN pin n (State.first bytes extns map)).memory a
      = (State.first bytes extns map).memory a :=
  runN_preserves_vectors pin n (State.first bytes extns map) a h


end MisaSpec

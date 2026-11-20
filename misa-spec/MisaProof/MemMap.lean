-- State.runN, which runN_caps lifts step_caps over, is defined alongside the vector table proofs
import MisaProof.Vector


/-
The capability map is configuration, not state.

A MemMap describes what the memory hardware provides, which the manual fixes for the lifetime of an
implementation rather than letting software change it. The model carries the map inside State so
that a serialized state round-trips the machine it was running on, which leaves open the question
of whether a program can reach it from there. The theorems below close that off:

- Every primitive that builds a new state leaves the capability map alone, including the ones that
  write memory and the ones that enter a trap.
- Executing any instruction, taking any fault, and entering an interrupt all leave it alone. This
  is proved over every instruction rather than over the ones that happen to touch memory, so a new
  instruction cannot quietly gain the ability to remap memory.

The result is step_caps, which says that no matter what a program executes it runs on the same
memory it started on.
-/


namespace MisaSpec


theorem State.writeReg_caps (q : State) (r : GpReg) (v : DataWord) :
    (q.writeReg r v).caps = q.caps := by
  cases r <;> rfl


theorem State.writeMem_caps (q : State) (a : AddrWord) (v : DataWord) :
    (q.writeMem a v).caps = q.caps := rfl


theorem State.writeFlags_caps (q : State) (f : CsrFlags) :
    (q.writeFlags f).caps = q.caps := rfl


theorem State.writeSpecial_caps (q : State) (c : CsrReg) (v : SpecWord) :
    (q.writeSpecial c v).caps = q.caps := by
  cases c
  case extns => simp only [State.writeSpecial]; split <;> rfl
  all_goals rfl


theorem State.faultAt_caps (q : State) (c : CsrCause) (loc : AddrWord) :
    (q.faultAt c loc).caps = q.caps := by
  simp only [State.faultAt]
  split
  all_goals rfl


theorem State.fault_caps (q : State) (c : CsrCause) : (q.fault c).caps = q.caps :=
  State.faultAt_caps q c q.pc


theorem State.interrupt_caps (q : State) : q.interrupt.caps = q.caps := by
  simp only [State.interrupt]
  split
  next => rfl
  next => exact State.faultAt_caps _ _ _


theorem State.next_caps (q : State) : q.next.caps = q.caps := by
  simp only [State.next]
  split
  next => exact State.fault_caps q _
  next => rfl


-- No instruction can change the capability map, whether it faults or completes
theorem decodeAndExecute_caps (q : State) : (decodeAndExecute q).caps = q.caps := by
  simp only [decodeAndExecute]
  split
  next => exact State.fault_caps q _
  next inst _ =>
    cases inst <;>
      simp only [executeHalt, executeSyscall, executeAdd, executeAdc, executeSub, executeSbb,
                 executeAnd, executeOr, executeXor, executeRrc, executeSet, executeLd, executeSt,
                 executeRsr, executeWsr, executeJal, executeJmp, executeJmpCsr] <;>
      repeat (any_goals split)
    all_goals simp only [State.next_caps, State.fault_caps, State.faultAt_caps,
                         State.writeReg_caps, State.writeSpecial_caps, State.writeFlags_caps,
                         State.writeMem_caps]
    all_goals rfl


theorem execute_caps (q : State) : (execute q).caps = q.caps := by
  simp only [execute]
  split
  next => exact State.fault_caps q _
  next => exact decodeAndExecute_caps q


-- A program cannot change the memory it is running on, no matter what it executes
theorem step_caps (q : State) : (step q).caps = q.caps := by
  simp only [step]
  split
  next => rfl
  next =>
    split
    next => exact State.interrupt_caps q
    next => exact execute_caps _


-- Nor can any run of a program, of any length, under any interrupt pin behaviour
theorem runN_caps (pin : Nat -> Bool) (n : Nat) (q : State) :
    (State.runN pin n q).caps = q.caps := by
  induction n generalizing pin q with
  | zero      => rfl
  | succ n ih =>
    simp only [State.runN]
    rw [ih]
    exact step_caps _


-- Reset installs the map the implementation was given, whether or not it could read the reset
-- vector out of it, and a model given no map provides the entire address range
theorem State.first_caps (bytes : ByteArray) (extns : SpecWord) (map : MemMap) :
    (State.first bytes extns map).caps = MemMap.toCaps map := by
  simp only [State.first]
  split
  next => rfl
  next => exact State.faultAt_caps _ _ _


theorem State.first_caps_default (bytes : ByteArray) (extns : SpecWord) (a : AddrWord) :
    (State.first bytes extns []).caps a = some MemCaps.rwx := by
  rw [State.first_caps bytes extns []]
  rfl


end MisaSpec

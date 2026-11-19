import MisaProof.Mpu


/-
The memory usage fault ordering.

The manual fixes a total order over the six memory usage faults and requires that, where several
conditions hold at once, the highest ranked one is the fault reported:

  1. PC overflow        2. Address overflow   3. Privileged access
  4. Unmapped address   5. Vector write       6. Invalid access

The first two describe a program counter or an address computation rather than a location. PC
overflow is decided in execute before a fetch is attempted, and address overflow is decided in the
load and store paths before there is an address whose properties could be examined. The remaining
four are decided together in State.accessFault, and the theorems below pin each one to the exact
conditions under which it is the fault reported, which is what makes the ordering checkable rather
than implicit in the order the tests happen to be written.
-/


namespace MisaSpec


-- The MPU constrains User Mode only, so in Machine Mode it grants every kind of access
theorem mpuCaps_allows_in_machine_mode (q : State) (acc : AccessKind) (a : AddrWord)
    (h : q.userMode = false) : (q.mpuCaps a).allows acc = true := by
  cases acc <;> simp [State.mpuCaps, h, MemCaps.allows, MemCaps.rwx]


-- Rank 3. An access the MPU does not permit reports a privileged access whatever else is wrong
-- with the address, so it outranks an unmapped address, a vector write, and an invalid access
theorem accessFault_privileged (q : State) (acc : AccessKind) (a : AddrWord)
    (h : (q.mpuCaps a).allows acc = false) :
    q.accessFault acc a = some (CsrCause.privilegedAccess acc) := by
  simp [State.accessFault, h]


-- Rank 4. Where the MPU permits the access but the implementation provides no memory, the fault
-- is an unmapped address, which outranks both faults below it
theorem accessFault_unmapped (q : State) (acc : AccessKind) (a : AddrWord)
    (hm : (q.mpuCaps a).allows acc = true) (h : q.caps a = none) :
    q.accessFault acc a = some (CsrCause.unmappedAddress acc) := by
  simp [State.accessFault, hm, h]


-- Rank 5. A store into the reset vector table at a mapped address the MPU permits reports a
-- vector write, which outranks the invalid access that read only vector memory would otherwise
-- report
theorem accessFault_vector (q : State) (a : AddrWord) (c : MemCaps)
    (hm : (q.mpuCaps a).allows AccessKind.write = true) (hc : q.caps a = some c)
    (hv : inVectorTable a = true) :
    q.accessFault AccessKind.write a = some (CsrCause.vectorWrite a) := by
  simp [State.accessFault, AccessKind.isVectorWrite, hm, hc, hv]


-- Rank 6. An invalid access is reported only when nothing above it applies: the MPU permits the
-- access, the implementation provides the memory, and the access is not a vector write
theorem accessFault_invalid (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hm : (q.mpuCaps a).allows acc = true) (hc : q.caps a = some c)
    (hv : acc.isVectorWrite a = false) (ha : c.allows acc = false) :
    q.accessFault acc a = some (CsrCause.invalidAccess acc) := by
  simp [State.accessFault, hm, hc, hv, ha]


-- An access at a mapped address which the MPU permits, which is not a vector write, and which the
-- memory itself allows raises no fault
theorem accessFault_none_of (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hc : q.caps a = some c) (hm : (q.mpuCaps a).allows acc = true)
    (hv : acc.isVectorWrite a = false) (ha : c.allows acc = true) :
    q.accessFault acc a = none := by
  simp [State.accessFault, hm, hc, hv, ha]


-- An access proceeds exactly when every one of the four conditions is clear
theorem accessFault_none_iff (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hc : q.caps a = some c) :
    q.accessFault acc a = none
      <-> ((q.mpuCaps a).allows acc = true /\ acc.isVectorWrite a = false
           /\ c.allows acc = true) := by
  constructor
  next =>
    intro h
    simp only [State.accessFault, hc] at h
    split at h
    next hp => exact absurd h (by simp)
    next hp =>
      split at h
      next => exact absurd h (by simp)
      next hv =>
        split at h
        next ha => exact And.intro (by simpa using hp) (And.intro (by simpa using hv) ha)
        next    => exact absurd h (by simp)
  next =>
    intro h
    exact accessFault_none_of q acc a c hc h.1 h.2.1 h.2.2


-- Instruction fetch tests the executable permission at both byte addresses
theorem fetch_tests_execute (q : State) (w : InstWord) (h : fetch q = Except.ok w) :
    q.accessFault AccessKind.execute q.pc = none
      /\ q.accessFault AccessKind.execute (q.pc + 1) = none := by
  simp only [fetch] at h
  split at h
  next => exact absurd h (by simp)
  next hlo =>
    split at h
    next => exact absurd h (by simp)
    next hhi => exact And.intro hlo hhi


-- The processor behaves as if the low byte is fetched first, so when both bytes of an instruction
-- word would fault it is the low byte's fault that is reported, even where the high byte's fault
-- would outrank it under the ordering above
theorem fetch_low_byte_first (q : State) (cause : CsrCause)
    (h : q.accessFault AccessKind.execute q.pc = some cause) :
    fetch q = Except.error cause := by
  simp [fetch, h]


-- The high byte's fault is reported only when the low byte raises none
theorem fetch_high_byte_second (q : State) (cause : CsrCause)
    (hlo : q.accessFault AccessKind.execute q.pc = none)
    (hhi : q.accessFault AccessKind.execute (q.pc + 1) = some cause) :
    fetch q = Except.error cause := by
  simp only [fetch, hlo, hhi]


-- A fetch that cannot complete discards the partially fetched word and raises its own fault
theorem fetch_fault_discards_word (q : State) (cause : CsrCause)
    (h : fetch q = Except.error cause) : decodeAndExecute q = q.fault cause :=
  decodeAndExecute_fetch_fault q cause h


end MisaSpec

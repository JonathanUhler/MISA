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

The ordering also says what a trap can fail on. State.accessFault is the MPU check in front of
State.mapFault, and a trap reads its handler address out of the vector table in Machine Mode, where
the MPU grants everything. So the read a trap performs consults the map alone, and the only causes
it can report are the two the map decides: an unmapped address and an invalid access.
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
  simp [State.accessFault, State.mapFault, hm, h]


-- Rank 5. A store into the reset vector table at a mapped address the MPU permits reports a
-- vector write, which outranks the invalid access that read only vector memory would otherwise
-- report
theorem accessFault_vector (q : State) (a : AddrWord) (c : MemCaps)
    (hm : (q.mpuCaps a).allows AccessKind.write = true) (hc : q.caps a = some c)
    (hv : inVectorTable a = true) :
    q.accessFault AccessKind.write a = some (CsrCause.vectorWrite a) := by
  simp [State.accessFault, State.mapFault, AccessKind.isVectorWrite, hm, hc, hv]


-- Rank 6. An invalid access is reported only when nothing above it applies: the MPU permits the
-- access, the implementation provides the memory, and the access is not a vector write
theorem accessFault_invalid (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hm : (q.mpuCaps a).allows acc = true) (hc : q.caps a = some c)
    (hv : acc.isVectorWrite a = false) (ha : c.allows acc = false) :
    q.accessFault acc a = some (CsrCause.invalidAccess acc) := by
  simp [State.accessFault, State.mapFault, hm, hc, hv, ha]


-- An access at a mapped address which the MPU permits, which is not a vector write, and which the
-- memory itself allows raises no fault
theorem accessFault_none_of (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hc : q.caps a = some c) (hm : (q.mpuCaps a).allows acc = true)
    (hv : acc.isVectorWrite a = false) (ha : c.allows acc = true) :
    q.accessFault acc a = none := by
  simp [State.accessFault, State.mapFault, hm, hc, hv, ha]


-- An access proceeds exactly when every one of the four conditions is clear
theorem accessFault_none_iff (q : State) (acc : AccessKind) (a : AddrWord) (c : MemCaps)
    (hc : q.caps a = some c) :
    q.accessFault acc a = none
      <-> ((q.mpuCaps a).allows acc = true /\ acc.isVectorWrite a = false
           /\ c.allows acc = true) := by
  constructor
  next =>
    intro h
    simp only [State.accessFault, State.mapFault, hc] at h
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


-- In Machine Mode the MPU grants everything, so an access check reduces to the memory map's own
-- check. This is why a trap, which drops to Machine Mode before it loads a handler address, has
-- only the map standing between it and the vector table.
theorem accessFault_eq_mapFault_in_machine_mode (q : State) (acc : AccessKind) (a : AddrWord)
    (h : q.userMode = false) : q.accessFault acc a = q.mapFault acc a := by
  simp [State.accessFault, mpuCaps_allows_in_machine_mode q acc a h]


-- The map can refuse a read in exactly two ways: it provides no memory at the address, or it
-- provides memory that is not readable
theorem mapFault_read_kinds (q : State) (a : AddrWord) (c : CsrCause)
    (h : q.mapFault AccessKind.read a = some c) :
    c = CsrCause.unmappedAddress AccessKind.read
      \/ c = CsrCause.invalidAccess AccessKind.read := by
  simp only [State.mapFault] at h
  split at h
  next =>
    have he : CsrCause.unmappedAddress AccessKind.read = c := by simpa using h
    exact Or.inl he.symm
  next =>
    split at h
    next hv => simp [AccessKind.isVectorWrite] at hv
    next =>
      split at h
      next => exact absurd h (by simp)
      next =>
        have he : CsrCause.invalidAccess AccessKind.read = c := by simpa using h
        exact Or.inr he.symm


-- A vector read the map refuses therefore reports an unmapped address or an invalid access, both
-- naming a read. A privileged access cannot arise, since the read happens in Machine Mode, and nor
-- can a vector write, which only a store can raise. These two are the only causes a trap which
-- cannot read the fault handler address can record before it halts.
theorem readVector_error_kinds (q : State) (lo : AddrWord) (c : CsrCause)
    (h : q.readVector lo = Except.error c) :
    c = CsrCause.unmappedAddress AccessKind.read
      \/ c = CsrCause.invalidAccess AccessKind.read := by
  cases readVector_error_cause q lo c h with
  | inl hlo => exact mapFault_read_kinds q lo c hlo
  | inr hhi => exact mapFault_read_kinds q (lo + 1) c hhi


-- The map allows a read exactly when it provides memory at the address and that memory is
-- readable
theorem mapFault_read_eq_none_iff (q : State) (a : AddrWord) :
    q.mapFault AccessKind.read a = none
      <-> Exists fun c : MemCaps => q.caps a = some c /\ c.r = true := by
  simp only [State.mapFault]
  cases hc : q.caps a with
  | none   => simp
  | some c => cases hr : c.r <;> simp [AccessKind.isVectorWrite, MemCaps.allows, hr]


-- A vector read succeeds exactly when the map allows a read at both bytes of the vector
theorem readVector_eq_ok_iff (q : State) (lo : AddrWord) :
    (Exists fun t : AddrWord => q.readVector lo = Except.ok t)
      <-> (q.mapFault AccessKind.read lo = none
           /\ q.mapFault AccessKind.read (lo + 1) = none) := by
  simp only [State.readVector]
  cases h1 : q.mapFault AccessKind.read lo with
  | some c1 => simp
  | none    => cases h2 : q.mapFault AccessKind.read (lo + 1) with
    | some c2 => simp
    | none    => simp


-- A trap can load the fault handler address exactly when the memory map provides readable memory
-- at both bytes of the fault vector. That is the whole condition: the vector sits in the reset
-- vector table, which no store can change, and the MPU does not apply to the read a trap performs.
theorem faultVectorReadable_iff_caps (q : State) :
    q.faultVectorReadable
      <-> ((Exists fun c1 : MemCaps => q.caps 0xFFFC = some c1 /\ c1.r = true)
           /\ (Exists fun c2 : MemCaps => q.caps 0xFFFD = some c2 /\ c2.r = true)) := by
  have ha : (0xFFFC : AddrWord) + 1 = 0xFFFD := by bv_decide
  constructor
  next =>
    intro h
    have hok : Exists fun t : AddrWord => q.readVector 0xFFFC = Except.ok t :=
      Exists.intro q.faultVector h
    have h2 := (readVector_eq_ok_iff q 0xFFFC).mp hok
    rw [ha] at h2
    exact And.intro ((mapFault_read_eq_none_iff q 0xFFFC).mp h2.1)
                    ((mapFault_read_eq_none_iff q 0xFFFD).mp h2.2)
  next =>
    intro h
    have h1 := (mapFault_read_eq_none_iff q 0xFFFC).mpr h.1
    have h2 := (mapFault_read_eq_none_iff q 0xFFFD).mpr h.2
    show q.readVector 0xFFFC = Except.ok q.faultVector
    simp only [State.readVector, State.faultVector, ha, h1, h2]


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

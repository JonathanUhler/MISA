import Std.Tactic.BVDecide
import MisaSpec


namespace MisaSpec


-- Decidable equality for the model types that derive only Repr, needed before decide can discharge
-- the finite enumeration goals in this library. GpReg must precede Inst, which contains it.
deriving instance DecidableEq for GpReg
deriving instance DecidableEq for FaultKind
deriving instance DecidableEq for CsrFlags
deriving instance DecidableEq for CsrExtns
deriving instance DecidableEq for Inst


-- Every four bit word
def HalfWord.all : List HalfWord :=
  (List.range 16).map (BitVec.ofNat 4)


-- Every three bit word
def CmpWord.all : List CmpWord :=
  (List.range 8).map (BitVec.ofNat 3)


-- Every eight bit word
def DataWord.all : List DataWord :=
  (List.range 256).map (BitVec.ofNat 8)


-- A word is the encoding of its own value, since that value is below the modulus of its width
theorem ofNat_toNat_self {w : Nat} (x : BitVec w) : BitVec.ofNat w x.toNat = x :=
  BitVec.eq_of_toNat_eq (by rw [BitVec.toNat_ofNat, Nat.mod_eq_of_lt x.isLt])


-- Every word whose value is below k appears in the enumeration of the first k naturals, which is
-- the shape each of the enumerations above has
theorem contains_ofNat_range {w : Nat} {k : Nat} (x : BitVec w) (h : x.toNat < k) :
    ((List.range k).map (BitVec.ofNat w)).contains x = true := by
  simpa using List.mem_map.mpr
    (Exists.intro x.toNat (And.intro (List.mem_range.mpr h) (ofNat_toNat_self x)))


-- A predicate that holds on every word in the enumeration of the first k naturals holds on every
-- word whose value is below k
theorem forall_ofNat_range {w : Nat} {k : Nat} (p : BitVec w -> Bool)
    (h : ((List.range k).map (BitVec.ofNat w)).all p = true) (x : BitVec w) (hx : x.toNat < k) :
    p x = true :=
  List.all_eq_true.mp h x (List.mem_map.mpr
    (Exists.intro x.toNat (And.intro (List.mem_range.mpr hx) (ofNat_toNat_self x))))


-- Every HalfWord appears in HalfWord.all
theorem HalfWord.mem_all (n : HalfWord) : HalfWord.all.contains n = true :=
  contains_ofNat_range (k := 16) n n.isLt


-- Every CmpWord appears in CmpWord.all
theorem CmpWord.mem_all (n : CmpWord) : CmpWord.all.contains n = true :=
  contains_ofNat_range (k := 8) n n.isLt


-- Every DataWord appears in DataWord.all
theorem DataWord.mem_all (n : DataWord) : DataWord.all.contains n = true :=
  contains_ofNat_range (k := 256) n n.isLt


-- A predicate that holds on every element of HalfWord.all holds on every HalfWord
theorem forall_halfWord (p : HalfWord -> Bool) (h : HalfWord.all.all p = true) (n : HalfWord) :
    p n = true :=
  forall_ofNat_range (k := 16) p h n n.isLt


-- A predicate that holds on every element of CmpWord.all holds on every CmpWord
theorem forall_cmpWord (p : CmpWord -> Bool) (h : CmpWord.all.all p = true) (n : CmpWord) :
    p n = true :=
  forall_ofNat_range (k := 8) p h n n.isLt


-- A predicate that holds on every element of DataWord.all holds on every DataWord
theorem forall_dataWord (p : DataWord -> Bool) (h : DataWord.all.all p = true) (n : DataWord) :
    p n = true :=
  forall_ofNat_range (k := 256) p h n n.isLt


-- The top bit of the seventeen bit sum is the carry out of a sixteen bit addition
theorem bit16_eq_ge (x : BitVec 17) : x.getLsbD 16 = decide (65536 <= x.toNat) := by
  have h : x.getLsbD 16 = BitVec.ule 65536#17 x := by bv_decide
  rw [h]
  simp [BitVec.ule]


-- Below the last instruction slot the link register computation stays inside the address space,
-- so a JAL or SYSCALL records the following instruction rather than raising an address overflow
theorem linkAddr_of_lt (pc : AddrWord) (h : pc < 0xFFFE#16) : linkAddr pc = some (pc + 2) := by
  simp only [linkAddr]
  split
  next hlt => exact absurd hlt (by bv_decide)
  next      => rfl


-- The fault handler address held in the reset vector table
def State.faultVector (q : State) : AddrWord :=
  q.memory 0xFFFD ++ q.memory 0xFFFC


-- The syscall handler address held in the reset vector table
def State.syscallVector (q : State) : AddrWord :=
  q.memory 0xFFFB ++ q.memory 0xFFFA


-- The interrupt handler address held in the reset vector table
def State.intVector (q : State) : AddrWord :=
  q.memory 0xFFF9 ++ q.memory 0xFFF8


-- The reset handler address held in the reset vector table
def State.resetVector (q : State) : AddrWord :=
  q.memory 0xFFFF ++ q.memory 0xFFFE


-- Whether an Inst is a SYSCALL
def Inst.isSyscall : Inst -> Bool
  | .syscall _ => true
  | _          => false


-- Whether every extension enabled in the first CsrExtns is also enabled in the second
def CsrExtns.le (e1 : CsrExtns) (e2 : CsrExtns) : Prop :=
  (e1.dynamichw = true -> e2.dynamichw = true)
    /\ (e1.syscall   = true -> e2.syscall   = true)
    /\ (e1.privilege = true -> e2.privilege = true)
    /\ (e1.interrupt = true -> e2.interrupt = true)


-- The flags produced by SUB and by the CMP pseudo-instruction, without the state plumbing
def subFlags (a : DataWord) (b : DataWord) : CsrFlags :=
  (aluAdd a (~~~ b) true).2


-- The flags produced by ADD, without the state plumbing
def addFlags (a : DataWord) (b : DataWord) : CsrFlags :=
  (aluAdd a b false).2


end MisaSpec

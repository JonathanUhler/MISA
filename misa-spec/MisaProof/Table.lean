import MisaProof.Encoding


/-
The bridge between the tables the model stores its components in and the finite maps they stand
for.

MisaSpec.Table holds the register file, the special registers, the region registers, and memory as
tables indexed by the encoding of the component being named, for the cost reasons recorded there.
The proof layer wants to see those components as functions, which takes two facts: a write agrees
with the state before it everywhere but at the index written, and a table built from a function
reads back as that function. These are the only places a proof needs to know that a table is what
is underneath.

All four are simp lemmas, so a proof that unfolds a write and the accessor that reads it back lands
on the if-then-else the write is written as without naming anything here.
-/


namespace MisaSpec


-- A write is readable back at the index written and leaves every other index alone. Both indices
-- are words, so the proof carries the comparison between them through the comparison between the
-- naturals they index by.
@[simp] theorem Table.get_set {α : Type} {w : Nat} (t : Table α w) (i : BitVec w) (j : BitVec w)
    (v : α) : (t.set i v).get j = if j == i then v else t.get j := by
  simp only [Table.get, Table.set]
  by_cases h : j = i
  next => simp [h]
  next =>
    have hn : Ne i.toNat j.toNat := fun he => h (BitVec.eq_of_toNat_eq he.symm)
    simp [h, hn]


@[simp] theorem Table.get_ofFn {α : Type} {w : Nat} (f : BitVec w -> α) (i : BitVec w) :
    (Table.ofFn f).get i = f i := by
  simp [Table.get, Table.ofFn]


-- A register table built from a function reads back as that function, since every four bit word
-- decodes to the register that encodes it
@[simp] theorem regsOfFn_get (f : GpReg -> DataWord) (r : GpReg) :
    (regsOfFn f).get r.enc = f r := by
  simp [regsOfFn, GpReg.dec_enc]


-- A special register table built from a function reads back as that function at every encoding.
-- The entries that are not encodings are the ones this says nothing about.
@[simp] theorem csrsOfFn_get (f : CsrReg -> SpecWord) (c : CsrReg) :
    (csrsOfFn f).get c.enc = f c := by
  simp [csrsOfFn, CsrReg.dec_enc]


/-
What each write does to the component that reads it back.

These are the lemmas the rest of the proof layer meets. Each has the shape the write would have had
if the component were a function updated by a closure, so a proof that unfolds a write lands on an
if-then-else over the index written and never has to mention a table. Keeping them keyed on the
accessor rather than unfolding it is what lets a hypothesis stated about q.special or q.register
still apply to the state after a write.
-/


@[simp] theorem State.register_setReg (q : State) (r : GpReg) (r' : GpReg) (v : DataWord) :
    (q.setReg r v).register r' = if r'.enc == r.enc then v else q.register r' := by
  simp [State.setReg, State.register]


@[simp] theorem State.special_setCsr (q : State) (c : CsrReg) (c' : CsrReg) (v : SpecWord) :
    (q.setCsr c v).special c' = if c'.enc == c.enc then v else q.special c' := by
  simp [State.setCsr, State.special]


@[simp] theorem State.mpuRegion_setMpu (q : State) (i : BitVec 2) (j : BitVec 2) (v : SpecWord) :
    (q.setMpu i v).mpuRegion j = if j == i then v else q.mpuRegion j := by
  simp [State.setMpu, State.mpuRegion]


@[simp] theorem State.memory_setMem (q : State) (a : AddrWord) (a' : AddrWord) (v : DataWord) :
    (q.setMem a v).memory a' = if a' == a then v else q.memory a' := by
  simp [State.setMem, State.memory]


-- A flags write stores FLAGS and refreshes the cached comparisons together, and so is the one
-- write that does not reduce to a single table store. It gets the same treatment so that proofs
-- reach it without unfolding an accessor either.
@[simp] theorem State.special_writeFlags (q : State) (f : CsrFlags) (c : CsrReg) :
    (q.writeFlags f).special c = if c.enc == CsrReg.flags.enc then f.enc else q.special c := by
  simp [State.writeFlags, State.special, State.setCsr]

@[simp] theorem State.check_writeFlags (q : State) (f : CsrFlags) (cmp : CmpFlag) :
    (q.writeFlags f).check cmp = CsrFlags.eval f cmp := rfl


-- Each write reaches one component, so it leaves the others alone, along with the memory map, the
-- cached comparisons and the program counter
@[simp] theorem State.special_setReg (q : State) (r : GpReg) (v : DataWord) (c : CsrReg) :
    (q.setReg r v).special c = q.special c := rfl

@[simp] theorem State.memory_setReg (q : State) (r : GpReg) (v : DataWord) (a : AddrWord) :
    (q.setReg r v).memory a = q.memory a := rfl

@[simp] theorem State.mpuRegion_setReg (q : State) (r : GpReg) (v : DataWord) (i : BitVec 2) :
    (q.setReg r v).mpuRegion i = q.mpuRegion i := rfl

@[simp] theorem State.caps_setReg (q : State) (r : GpReg) (v : DataWord) :
    (q.setReg r v).caps = q.caps := rfl

@[simp] theorem State.check_setReg (q : State) (r : GpReg) (v : DataWord) (cmp : CmpFlag) :
    (q.setReg r v).check cmp = q.check cmp := rfl

@[simp] theorem State.pc_setReg (q : State) (r : GpReg) (v : DataWord) :
    (q.setReg r v).pc = q.pc := rfl

@[simp] theorem State.register_setCsr (q : State) (c : CsrReg) (v : SpecWord) (r : GpReg) :
    (q.setCsr c v).register r = q.register r := rfl

@[simp] theorem State.memory_setCsr (q : State) (c : CsrReg) (v : SpecWord) (a : AddrWord) :
    (q.setCsr c v).memory a = q.memory a := rfl

@[simp] theorem State.mpuRegion_setCsr (q : State) (c : CsrReg) (v : SpecWord) (i : BitVec 2) :
    (q.setCsr c v).mpuRegion i = q.mpuRegion i := rfl

@[simp] theorem State.caps_setCsr (q : State) (c : CsrReg) (v : SpecWord) :
    (q.setCsr c v).caps = q.caps := rfl

@[simp] theorem State.check_setCsr (q : State) (c : CsrReg) (v : SpecWord) (cmp : CmpFlag) :
    (q.setCsr c v).check cmp = q.check cmp := rfl

@[simp] theorem State.pc_setCsr (q : State) (c : CsrReg) (v : SpecWord) :
    (q.setCsr c v).pc = q.pc := rfl

@[simp] theorem State.register_setMpu (q : State) (i : BitVec 2) (v : SpecWord) (r : GpReg) :
    (q.setMpu i v).register r = q.register r := rfl

@[simp] theorem State.special_setMpu (q : State) (i : BitVec 2) (v : SpecWord) (c : CsrReg) :
    (q.setMpu i v).special c = q.special c := rfl

@[simp] theorem State.memory_setMpu (q : State) (i : BitVec 2) (v : SpecWord) (a : AddrWord) :
    (q.setMpu i v).memory a = q.memory a := rfl

@[simp] theorem State.caps_setMpu (q : State) (i : BitVec 2) (v : SpecWord) :
    (q.setMpu i v).caps = q.caps := rfl

@[simp] theorem State.check_setMpu (q : State) (i : BitVec 2) (v : SpecWord) (cmp : CmpFlag) :
    (q.setMpu i v).check cmp = q.check cmp := rfl

@[simp] theorem State.pc_setMpu (q : State) (i : BitVec 2) (v : SpecWord) :
    (q.setMpu i v).pc = q.pc := rfl

@[simp] theorem State.register_setMem (q : State) (a : AddrWord) (v : DataWord) (r : GpReg) :
    (q.setMem a v).register r = q.register r := rfl

@[simp] theorem State.special_setMem (q : State) (a : AddrWord) (v : DataWord) (c : CsrReg) :
    (q.setMem a v).special c = q.special c := rfl

@[simp] theorem State.mpuRegion_setMem (q : State) (a : AddrWord) (v : DataWord) (i : BitVec 2) :
    (q.setMem a v).mpuRegion i = q.mpuRegion i := rfl

@[simp] theorem State.caps_setMem (q : State) (a : AddrWord) (v : DataWord) :
    (q.setMem a v).caps = q.caps := rfl

@[simp] theorem State.check_setMem (q : State) (a : AddrWord) (v : DataWord) (cmp : CmpFlag) :
    (q.setMem a v).check cmp = q.check cmp := rfl

@[simp] theorem State.pc_setMem (q : State) (a : AddrWord) (v : DataWord) :
    (q.setMem a v).pc = q.pc := rfl

@[simp] theorem State.register_writeFlags (q : State) (f : CsrFlags) (r : GpReg) :
    (q.writeFlags f).register r = q.register r := rfl

@[simp] theorem State.memory_writeFlags (q : State) (f : CsrFlags) (a : AddrWord) :
    (q.writeFlags f).memory a = q.memory a := rfl

@[simp] theorem State.mpuRegion_writeFlags (q : State) (f : CsrFlags) (i : BitVec 2) :
    (q.writeFlags f).mpuRegion i = q.mpuRegion i := rfl


end MisaSpec

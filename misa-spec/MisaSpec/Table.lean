import MisaSpec.Basic
import MisaSpec.GpReg
import MisaSpec.CsrReg


namespace MisaSpec


structure Table (α : Type) (w : Nat) where
  entries : Vector α (2 ^ w)


def Table.get {α : Type} {w : Nat} (t : Table α w) (i : BitVec w) : α :=
  t.entries[i.toFin]


def Table.set {α : Type} {w : Nat} (t : Table α w) (i : BitVec w) (v : α) : Table α w :=
  { entries := t.entries.set i.toNat v i.isLt }


def Table.ofFn {α : Type} {w : Nat} (f : BitVec w -> α) : Table α w :=
  { entries := Vector.ofFn (fun i => f (BitVec.ofFin i)) }


def regsOfFn (f : GpReg -> DataWord) : Table DataWord 4 :=
  Table.ofFn (fun n => f ((GpReg.dec n).getD .r0))


def csrsOfFn (f : CsrReg -> SpecWord) : Table SpecWord 4 :=
  Table.ofFn (fun n => ((CsrReg.dec n).map f).getD 0x0000#16)


end MisaSpec

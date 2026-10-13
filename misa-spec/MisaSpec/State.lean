import MisaSpec.Basic
import MisaSpec.GpReg
import MisaSpec.CsrReg
import MisaSpec.CmpFlag


namespace MisaSpec


structure State where
  register  : GpReg -> DataWord
  special   : CsrReg -> SpecWord
  memory    : AddrWord -> DataWord
  check     : CmpFlag -> Bool
  pc        : AddrWord
  inSyscall : Bool
  halted    : Bool


def State.writeReg (q : State) (r : GpReg) (v : DataWord) : State :=
  match r with
  | .r0 => q
  | _   => { q with register := fun r' => if r'.enc == r.enc then v else q.register r' }


def State.writeMem (q : State) (addr : AddrWord) (v : DataWord) : State :=
  { q with memory := fun a => if a == addr then v else q.memory a }


def State.writeFlags (q : State) (f : CsrFlags) : State :=
  { q with
    special := fun c => if c.enc == CsrReg.flags.enc then f.enc else q.special c,
    check   := CsrFlags.eval f }


def State.writeSpecial (q : State) (c : CsrReg) (v : SpecWord) : State :=
  match c with
  | .extns => q
  | .flags => q.writeFlags (CsrFlags.dec v)
  | _      => { q with special := fun c' => if c'.enc == c.enc then v else q.special c' }


def State.fault (q : State) (cause : CsrCause) : State :=
  let q := q.writeSpecial .cause cause.enc
  { q with pc := q.memory 0xFFFD ++ q.memory 0xFFFC }


def State.first (bytes : ByteArray) : State :=
  let mem : AddrWord -> DataWord := fun a =>
    let i := a.toNat
    if i < bytes.size then BitVec.ofNat 8 (bytes.get! i).toNat else 0
  { register  := fun _ => 0x00,
    special   := fun _ => 0x00,
    memory    := mem,
    check     := CsrFlags.eval (CsrFlags.dec 0x0000),
    pc        := mem 0xFFFF ++ mem 0xFFFE,
    inSyscall := false,
    halted    := false }


def State.next (q : State) : State :=
  let pc' := q.pc + 2
  if pc' < q.pc then
    q.fault CsrCause.pcOverflow
  else
    { q with pc := pc' }


end MisaSpec

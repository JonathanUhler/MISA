import MisaSpec.Basic
import MisaSpec.GpReg
import MisaSpec.CsrReg
import MisaSpec.CmpFlag
import MisaSpec.MemMap
import MisaSpec.Mpu


namespace MisaSpec


structure State where
  register     : GpReg -> DataWord
  special      : CsrReg -> SpecWord
  memory       : AddrWord -> DataWord
  caps         : AddrWord -> Option MemCaps
  mpuRegion    : BitVec 2 -> SpecWord
  check        : CmpFlag -> Bool
  pc           : AddrWord
  inSyscall    : Bool
  inInterrupt  : Bool
  intPending   : Bool
  intRscratch  : SpecWord
  intFlags     : SpecWord
  syscallPrivs : SpecWord
  intPrivs     : SpecWord
  halted       : Bool


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
  let v := v &&& c.writeMask
  match c with
  | .extns =>
    let cur := q.special .extns
    if (CsrExtns.dec cur).dynamichw then
      let v' := (v &&& 0xFFFE#16) ||| (cur &&& 0x0001#16)
      { q with special := fun c' => if c'.enc == CsrReg.extns.enc then v' else q.special c' }
    else
      q
  | .flags => q.writeFlags (CsrFlags.dec v)
  | .mpucf =>
    let idx := (MpuRegion.dec v).index
    { q with
      special   := fun c' => if c'.enc == CsrReg.mpucf.enc then v else q.special c',
      mpuRegion := fun i => if i == idx then v else q.mpuRegion i }
  | _      => { q with special := fun c' => if c'.enc == c.enc then v else q.special c' }


def linkAddr (pc : AddrWord) : Option AddrWord :=
  let next := pc + 2
  if next < pc then none else some next


def State.userMode (q : State) : Bool :=
  (CsrExtns.dec (q.special .extns)).privilege && (q.special .privs).getLsbD 0


def State.mpuCaps (q : State) (a : AddrWord) : MemCaps :=
  if q.userMode then Mpu.caps q.mpuRegion a else MemCaps.rwx


def State.mapFault (q : State) (acc : AccessKind) (a : AddrWord) : Option CsrCause :=
  match q.caps a with
  | none   => some (CsrCause.unmappedAddress acc)
  | some c =>
    if acc.isVectorWrite a then
      some (CsrCause.vectorWrite a)
    else if c.allows acc then
      none
    else
      some (CsrCause.invalidAccess acc)


def State.accessFault (q : State) (acc : AccessKind) (a : AddrWord) : Option CsrCause :=
  if !((q.mpuCaps a).allows acc) then
    some (CsrCause.privilegedAccess acc)
  else
    q.mapFault acc a


def State.readVector (q : State) (lo : AddrWord) : Except CsrCause AddrWord :=
  match q.mapFault .read lo with
  | some cause => .error cause
  | none       =>
    match q.mapFault .read (lo + 1) with
    | some cause => .error cause
    | none       => .ok (q.memory (lo + 1) ++ q.memory lo)


def State.privsUpdate (q : State) (v : SpecWord) : SpecWord :=
  if (CsrExtns.dec (q.special .extns)).privilege then v else q.special .privs


def State.faultAt (q : State) (cause : CsrCause) (loc : AddrWord) : State :=
  let cleared : SpecWord := q.privsUpdate 0x0000
  match q.readVector 0xFFFC with
  | .ok target =>
    let q := ((q.writeSpecial .cause cause.enc).writeSpecial .privs cleared).writeSpecial .fault loc
    { q with pc := target }
  | .error c   =>
    let q := ((q.writeSpecial .cause c.enc).writeSpecial .privs cleared).writeSpecial .fault 0xFFFC
    { q with halted := true }


def State.fault (q : State) (cause : CsrCause) : State :=
  q.faultAt cause q.pc


def State.init (bytes : ByteArray) (extns : SpecWord) (map : MemMap) : State :=
  let mem : AddrWord -> DataWord := fun a =>
    let i := a.toNat
    if i < bytes.size then BitVec.ofNat 8 (bytes.get! i).toNat else 0
  { register     := fun _ => 0x00#8,
    special      := fun c =>
      if c.enc == CsrReg.extns.enc then extns &&& CsrReg.extns.writeMask else 0x0000#16,
    memory       := mem,
    caps         := MemMap.toCaps map,
    mpuRegion    := fun _ => 0x0000#16,
    check        := CsrFlags.eval (CsrFlags.dec 0x0000#16),
    pc           := 0x0000#16,
    inSyscall    := false,
    inInterrupt  := false,
    intPending   := false,
    intRscratch  := 0x0000#16,
    intFlags     := 0x0000#16,
    syscallPrivs := 0x0000#16,
    intPrivs     := 0x0000#16,
    halted       := false }


def State.first (bytes : ByteArray) (extns : SpecWord) (map : MemMap) : State :=
  let q := State.init bytes extns map
  match q.readVector 0xFFFE with
  | .ok target => { q with pc := target }
  | .error c   => q.faultAt c 0xFFFE


def State.next (q : State) : State :=
  let pc' := q.pc + 2
  if pc' < q.pc then
    q.fault CsrCause.pcOverflow
  else
    { q with pc := pc' }


def State.canInterrupt (q : State) : Bool :=
  (CsrExtns.dec (q.special .extns)).interrupt && !q.inInterrupt


def State.interrupt (q : State) : State :=
  let saved : SpecWord := q.special .privs
  let cleared : SpecWord := q.privsUpdate 0x0000
  match q.readVector 0xFFF8 with
  | .ok target =>
    { ((q.writeSpecial .retir q.pc).writeSpecial .privs cleared) with
      intRscratch := q.register .rscratch0 ++ q.register .rscratch1,
      intFlags    := q.special .flags,
      intPrivs    := saved,
      inInterrupt := true,
      intPending  := false,
      pc          := target }
  | .error c   => { q with intPending := false }.faultAt c 0xFFF8


end MisaSpec

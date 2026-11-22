import MisaSpec.Basic
import MisaSpec.GpReg
import MisaSpec.CsrReg
import MisaSpec.CmpFlag
import MisaSpec.MemMap
import MisaSpec.Mpu
import MisaSpec.Table


namespace MisaSpec


structure State where
  regs         : Table DataWord 4
  csrs         : Table SpecWord 4
  mem          : Table DataWord 16
  caps         : AddrWord -> Option MemCaps
  mpu          : Table SpecWord 2
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


def State.register (q : State) (r : GpReg) : DataWord :=
  q.regs.get r.enc


def State.special (q : State) (c : CsrReg) : SpecWord :=
  q.csrs.get c.enc


def State.mpuRegion (q : State) (i : BitVec 2) : SpecWord :=
  q.mpu.get i


def State.memory (q : State) (a : AddrWord) : DataWord :=
  q.mem.get a


def State.setReg (q : State) (r : GpReg) (v : DataWord) : State :=
  { q with regs := q.regs.set r.enc v }


def State.setCsr (q : State) (c : CsrReg) (v : SpecWord) : State :=
  { q with csrs := q.csrs.set c.enc v }


def State.setMpu (q : State) (i : BitVec 2) (v : SpecWord) : State :=
  { q with mpu := q.mpu.set i v }


def State.setMem (q : State) (a : AddrWord) (v : DataWord) : State :=
  { q with mem := q.mem.set a v }


def State.writeReg (q : State) (r : GpReg) (v : DataWord) : State :=
  match r with
  | .r0 => q
  | _   => q.setReg r v


def State.writeMem (q : State) (addr : AddrWord) (v : DataWord) : State :=
  q.setMem addr v


def State.writeFlags (q : State) (f : CsrFlags) : State :=
  { (q.setCsr CsrReg.flags f.enc) with check := CsrFlags.eval f }


def State.writeSpecial (q : State) (c : CsrReg) (v : SpecWord) : State :=
  let v := v &&& c.writeMask
  match c with
  | .extns =>
    let cur := q.special .extns
    if (CsrExtns.dec cur).dynamichw then
      q.setCsr .extns ((v &&& 0xFFFE#16) ||| (cur &&& 0x0001#16))
    else
      q
  | .flags => q.writeFlags (CsrFlags.dec v)
  | .mpucf =>
    let idx := (MpuRegion.dec v).index
    (q.setCsr .mpucf v).setMpu idx v
  | _      => q.setCsr c v


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
  let mem : Table DataWord 16 := Table.ofFn (fun a =>
    let i := a.toNat
    if i < bytes.size then BitVec.ofNat 8 (bytes.get! i).toNat else 0)
  { regs         := regsOfFn (fun _ => 0x00#8),
    csrs         := csrsOfFn (fun c =>
      if c.enc == CsrReg.extns.enc then extns &&& CsrReg.extns.writeMask else 0x0000#16),
    mem          := mem,
    caps         := MemMap.toCaps map,
    mpu          := Table.ofFn (fun _ => 0x0000#16),
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

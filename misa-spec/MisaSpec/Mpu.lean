import MisaSpec.Basic
import MisaSpec.CsrReg
import MisaSpec.MemMap


namespace MisaSpec


def MemCaps.allows (c : MemCaps) : AccessKind -> Bool
  | .execute => c.x
  | .write   => c.w
  | .read    => c.r


def AccessKind.isVectorWrite (acc : AccessKind) (a : AddrWord) : Bool :=
  match acc with
  | .write => inVectorTable a
  | _      => false


structure MpuRegion where
  execute : Bool
  write   : Bool
  read    : Bool
  index   : BitVec 2
  size    : BitVec 3
  base    : DataWord
  deriving DecidableEq, Repr


def MpuRegion.enc (reg : MpuRegion) : SpecWord :=
  reg.base ++ reg.size ++ reg.index ++
    BitVec.ofBool reg.read ++ BitVec.ofBool reg.write ++ BitVec.ofBool reg.execute


def MpuRegion.dec (n : SpecWord) : MpuRegion :=
  { execute := n.getLsbD 0,
    write   := n.getLsbD 1,
    read    := n.getLsbD 2,
    index   := n.extractLsb' 3 2,
    size    := n.extractLsb' 5 3,
    base    := n.extractLsb' 8 8 }


def pageOf (a : AddrWord) : DataWord := a.extractLsb' 8 8


def MpuRegion.contains (reg : MpuRegion) (a : AddrWord) : Bool :=
  (pageOf a) >>> reg.size.setWidth 8 == reg.base >>> reg.size.setWidth 8


def MpuRegion.caps (reg : MpuRegion) : MemCaps :=
  { r := reg.read, w := reg.write, x := reg.execute }


def MpuRegion.indices : List (BitVec 2) := [0b00#2, 0b01#2, 0b10#2, 0b11#2]


def Mpu.caps (regs : BitVec 2 -> SpecWord) (a : AddrWord) : MemCaps :=
  match MpuRegion.indices.find? (fun i => (MpuRegion.dec (regs i)).contains a) with
  | some i => (MpuRegion.dec (regs i)).caps
  | none   => MemCaps.empty


end MisaSpec

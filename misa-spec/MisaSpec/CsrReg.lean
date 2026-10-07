import MisaSpec.Basic


namespace MisaSpec


inductive CsrReg where
  | saddr | raddr | flags | cause | extns
  deriving Repr


def CsrReg.all : List CsrReg :=
  [.saddr, .raddr, .flags, .cause, .extns]


def CsrReg.enc : CsrReg -> HalfWord
  | .saddr => 0b0001
  | .raddr => 0b0010
  | .flags => 0b0011
  | .cause => 0b0100
  | .extns => 0b0101


def CsrReg.dec (n : HalfWord) : Option CsrReg :=
  CsrReg.all.find? (·.enc == n)


structure CsrFlags where
  z : Bool
  c : Bool
  n : Bool
  v : Bool
  deriving Repr


def CsrFlags.enc (flags : CsrFlags) : SpecWord :=
  (BitVec.ofBool flags.v ++
   BitVec.ofBool flags.n ++
   BitVec.ofBool flags.c ++
   BitVec.ofBool flags.z).setWidth 16


def CsrFlags.dec (n : SpecWord) : CsrFlags :=
  { z := n.getLsbD 0, c := n.getLsbD 1, n := n.getLsbD 2, v := n.getLsbD 3 }


structure CsrExtns where
  dynamichw : Bool
  syscall   : Bool
  privilege : Bool
  interrupt : Bool


def CsrExtns.enc (extns : CsrExtns) : SpecWord :=
  (BitVec.ofBool extns.interrupt ++
   BitVec.ofBool extns.privilege ++
   BitVec.ofBool extns.syscall   ++
   BitVec.ofBool extns.dynamichw).setWidth 16


def CsrExtns.dec (n : SpecWord) : CsrExtns :=
  { dynamichw := n.getLsbD 0,
    syscall   := n.getLsbD 1,
    privilege := n.getLsbD 2,
    interrupt := n.getLsbD 2}


end MisaSpec

import MisaSpec.Basic
import MisaSpec.CmpFlag


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


def CsrFlags.eval (flags : CsrFlags) : CmpFlag -> Bool
  | .always       => true
  | .equal        => flags.z
  | .notEqual     => !flags.z
  | .greater      => !((flags.n != flags.v) || flags.z)
  | .less         => flags.n != flags.v
  | .greaterEqual => !(flags.n != flags.v)
  | .lessEqual    => (flags.n != flags.v) || flags.z


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
    interrupt := n.getLsbD 3 }


inductive FaultKind where
  -- Reason: None
  | none
  -- Reason: Instruction Usage
  | haltInstruction
  | illegalInstruction
  -- Reason: Memory Usage
  | addressOverflow
  | pcOverflow
  deriving Repr


def FaultKind.all : List FaultKind :=
  [.none, .haltInstruction, .illegalInstruction, .addressOverflow, .pcOverflow]


def FaultKind.enc : FaultKind -> BitVec 6
  | .none               => 0b000000#6
  | .haltInstruction    => 0b000001#6
  | .illegalInstruction => 0b001001#6
  | .addressOverflow    => 0b000010#6
  | .pcOverflow         => 0b001010#6


def FaultKind.dec (n : BitVec 6) : Option FaultKind :=
  FaultKind.all.find? (·.enc == n)


structure CsrCause where
  kind   : FaultKind
  status : DataWord


def CsrCause.enc (cause : CsrCause) : SpecWord :=
  cause.status ++ 0b00#2 ++ cause.kind.enc


def CsrCause.make (kind : FaultKind) : SpecWord :=
  let cause : CsrCause := { kind := kind, status := 0b00000000#8 }
  cause.enc


def CsrCause.dec (n : SpecWord) : Option CsrCause :=
  match FaultKind.dec (n.extractLsb' 0 6) with
  | none      => none
  | some kind => some { kind := kind, status := n.extractLsb' 8 8 }


end MisaSpec

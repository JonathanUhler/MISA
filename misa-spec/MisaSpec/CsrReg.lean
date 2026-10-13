import MisaSpec.Basic
import MisaSpec.CmpFlag


namespace MisaSpec


inductive CsrReg where
  | saddr | raddr | flags | cause | extns
  -- System call extension
  | retsc
  deriving DecidableEq, Repr


def CsrReg.all : List CsrReg :=
  [
    .saddr, .raddr, .flags, .cause, .extns,
    .retsc
  ]


def CsrReg.enc : CsrReg -> HalfWord
  | .saddr => 0b0001#4
  | .raddr => 0b0010#4
  | .flags => 0b0011#4
  | .cause => 0b0100#4
  | .extns => 0b0101#4
  -- System call extension
  | .retsc => 0b1000#4


def CsrReg.dec (n : HalfWord) : Option CsrReg :=
  CsrReg.all.find? (·.enc == n)


def CsrReg.name : CsrReg -> String
  | .saddr => "saddr"
  | .raddr => "raddr"
  | .flags => "flags"
  | .cause => "cause"
  | .extns => "extns"
  -- System call extension
  | .retsc => "retsc"


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


def CsrReg.availableUnder (csr : CsrReg) (extns : CsrExtns) : Bool :=
  match csr with
  -- System call extension
  | .retsc => extns.syscall
  | _      => true


inductive FaultKind where
  -- Reason: None
  | nothing
  -- Reason: Instruction Usage
  | haltInstruction
  | illegalInstruction
  | privilegedInstruction
  -- Reason: Memory Usage
  | addressOverflow
  | pcOverflow
  | privilegedAccess
  -- Reason: Control Event
  | improperReturnContext
  | conditionalContextReturn
  | improperSystemCall
  deriving Repr


def FaultKind.all : List FaultKind :=
  [
    .nothing,
    .haltInstruction, .illegalInstruction, .privilegedInstruction,
    .addressOverflow, .pcOverflow, .privilegedAccess,
    .improperReturnContext, .conditionalContextReturn, .improperSystemCall
  ]


def FaultKind.enc : FaultKind -> BitVec 6
  | .nothing                  => 0b000000#6
  | .haltInstruction          => 0b000001#6
  | .illegalInstruction       => 0b001001#6
  | .privilegedInstruction    => 0b010001#6
  | .addressOverflow          => 0b000010#6
  | .pcOverflow               => 0b001010#6
  | .privilegedAccess         => 0b010010#6
  | .improperReturnContext    => 0b000011#6
  | .conditionalContextReturn => 0b001011#6
  | .improperSystemCall       => 0b010011#6


def FaultKind.dec (n : BitVec 6) : Option FaultKind :=
  FaultKind.all.find? (·.enc == n)


structure CsrCause where
  kind   : FaultKind
  status : DataWord


def CsrCause.enc (cause : CsrCause) : SpecWord :=
  cause.status ++ 0b00#2 ++ cause.kind.enc


def CsrCause.nothing : CsrCause :=
  { kind := .nothing, status := 0b00000000#8 }

def CsrCause.haltInstruction (rs : DataWord) : CsrCause :=
  { kind := .haltInstruction, status := rs }

def CsrCause.illegalInstruction : CsrCause :=
  { kind := .illegalInstruction, status := 0b00000000#8 }

def CsrCause.privilegedInstruction : CsrCause :=
  { kind := .privilegedInstruction, status := 0b00000000#8 }

def CsrCause.addressOverflow : CsrCause :=
  { kind := .addressOverflow, status := 0b00000000#8 }

def CsrCause.pcOverflow : CsrCause :=
  { kind := .pcOverflow, status := 0b00000000#8 }

def CsrCause.privilegedAccess : CsrCause :=
  { kind := .privilegedAccess, status := 0b00000000#8 }

def CsrCause.improperReturnContext (inSyscall inInterrupt : Bool) (csr : CsrReg) : CsrCause :=
  { kind   := .improperReturnContext,
    status := csr.enc ++ 0b00#2 ++ BitVec.ofBool inInterrupt ++ BitVec.ofBool inSyscall }

def CsrCause.conditionalContextReturn (cmp : CmpFlag) (csr : CsrReg) : CsrCause :=
  { kind := .conditionalContextReturn, status := csr.enc ++ 0b0#1 ++ cmp.enc }

def CsrCause.improperSystemCall (inSyscall inInterrupt : Bool) : CsrCause :=
  { kind   := .improperSystemCall,
    status := 0b000000#6 ++ BitVec.ofBool inInterrupt ++ BitVec.ofBool inSyscall }


def CsrCause.dec (n : SpecWord) : Option CsrCause :=
  match FaultKind.dec (n.extractLsb' 0 6) with
  | none      => none
  | some kind => some { kind := kind, status := n.extractLsb' 8 8 }


end MisaSpec

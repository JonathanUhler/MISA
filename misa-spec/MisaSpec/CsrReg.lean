import MisaSpec.Basic
import MisaSpec.CmpFlag


namespace MisaSpec


inductive CsrReg where
  | saddr
  | raddr
  | flags
  | cause
  | extns
  | fault
  -- System call extension
  | retsc
  -- Interrupt extension
  | retir
  -- Privilege extension
  | privs
  | mpucf
  deriving DecidableEq, Repr


def CsrReg.all : List CsrReg :=
  [
    .saddr, .raddr, .flags, .cause, .extns, .fault,
    -- System call extension
    .retsc,
    -- Interrupt extension
    .retir,
    -- Privilege extension
    .privs, .mpucf
  ]


def CsrReg.enc : CsrReg -> HalfWord
  | .saddr => 0b0001#4
  | .raddr => 0b0010#4
  | .flags => 0b0011#4
  | .cause => 0b0100#4
  | .extns => 0b0101#4
  | .fault => 0b0110#4
  -- System call extension
  | .retsc => 0b1000#4
  -- Interrupt extension
  | .retir => 0b1001#4
  -- Privilege extension
  | .privs => 0b1010#4
  | .mpucf => 0b1011#4


def CsrReg.dec (n : HalfWord) : Option CsrReg :=
  CsrReg.all.find? (·.enc == n)


def CsrReg.name : CsrReg -> String
  | .saddr => "saddr"
  | .raddr => "raddr"
  | .flags => "flags"
  | .cause => "cause"
  | .extns => "extns"
  | .fault => "fault"
  -- System call extension
  | .retsc => "retsc"
  -- Interrupt extension
  | .retir => "retir"
  -- Privilege extension
  | .privs => "privs"
  | .mpucf => "mpucf"


structure CsrFlags where
  z : Bool
  c : Bool
  n : Bool
  v : Bool
  deriving Repr


def CsrFlags.enc (flags : CsrFlags) : SpecWord :=
  ( BitVec.ofBool flags.v ++
    BitVec.ofBool flags.n ++
    BitVec.ofBool flags.c ++
    BitVec.ofBool flags.z ).setWidth 16


def CsrFlags.dec (n : SpecWord) : CsrFlags :=
  { z := n.getLsbD 0, c := n.getLsbD 1, n := n.getLsbD 2, v := n.getLsbD 3 }


def CsrFlags.eval (flags : CsrFlags) : CmpFlag -> Bool
  | .always       => true
  | .equal        => flags.z
  | .notEqual     => !flags.z
  | .below        => !flags.c
  | .less         => flags.n != flags.v
  | .greaterEqual => !(flags.n != flags.v)
  | .aboveEqual   => flags.c


structure CsrExtns where
  dynamichw : Bool
  syscall   : Bool
  privilege : Bool
  interrupt : Bool


def CsrExtns.enc (extns : CsrExtns) : SpecWord :=
  ( BitVec.ofBool extns.interrupt ++
    BitVec.ofBool extns.privilege ++
    BitVec.ofBool extns.syscall   ++
    BitVec.ofBool extns.dynamichw ).setWidth 16


def CsrExtns.dec (n : SpecWord) : CsrExtns :=
  { dynamichw := n.getLsbD 0,
    syscall   := n.getLsbD 1,
    privilege := n.getLsbD 2,
    interrupt := n.getLsbD 3 }


def CsrReg.availableUnder (csr : CsrReg) (extns : CsrExtns) : Bool :=
  match csr with
  -- System call extension
  | .retsc => extns.syscall
  -- Interrupt extension
  | .retir => extns.interrupt
  -- Privilege extension
  | .privs => extns.privilege
  | .mpucf => extns.privilege
  | _      => true


def CsrReg.writeMask : CsrReg -> SpecWord
  | .saddr => 0xFFFF#16
  | .raddr => 0xFFFF#16
  | .flags => 0x000F#16
  | .cause => 0xFF3F#16
  | .extns => 0x000F#16
  | .fault => 0xFFFF#16
  -- System call extension
  | .retsc => 0xFFFF#16
  -- Interrupt extension
  | .retir => 0xFFFF#16
  -- Privilege extension
  | .privs => 0x0001#16
  | .mpucf => 0xFFFF#16


def CsrReg.userReadable : CsrReg -> Bool
  | .saddr | .raddr | .flags | .privs => true
  | _                                 => false


def CsrReg.userWritable : CsrReg -> Bool
  | .saddr | .raddr | .flags => true
  | _                        => false


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
  | vectorWrite
  | invalidAccess
  | unmappedAddress
  -- Reason: Control Event
  | improperReturnContext
  | conditionalContextReturn
  | improperSystemCall
  deriving Repr


def FaultKind.enc : FaultKind -> BitVec 6
  | .nothing                  => 0b000000#6
  | .haltInstruction          => 0b000001#6
  | .illegalInstruction       => 0b001001#6
  | .privilegedInstruction    => 0b010001#6
  | .addressOverflow          => 0b000010#6
  | .pcOverflow               => 0b001010#6
  | .privilegedAccess         => 0b010010#6
  | .vectorWrite              => 0b011010#6
  | .invalidAccess            => 0b100010#6
  | .unmappedAddress          => 0b101010#6
  | .improperReturnContext    => 0b000011#6
  | .conditionalContextReturn => 0b001011#6
  | .improperSystemCall       => 0b010011#6


inductive AccessKind where
  | execute
  | write
  | read
  deriving DecidableEq, Repr


def AccessKind.enc : AccessKind -> DataWord
  | .execute => 0b00000001#8
  | .write   => 0b00000010#8
  | .read    => 0b00000100#8


inductive OverflowKind where
  | access (acc : AccessKind)
  | saddr
  | link
  deriving DecidableEq, Repr


def OverflowKind.enc : OverflowKind -> DataWord
  | .access acc => acc.enc
  | .saddr      => 0b00001000#8
  | .link       => 0b00010000#8


structure CsrCause where
  kind   : FaultKind
  status : DataWord


def CsrCause.enc (cause : CsrCause) : SpecWord :=
  cause.status ++ 0b00#2 ++ cause.kind.enc


def CsrCause.haltInstruction (rs : DataWord) : CsrCause :=
  { kind := .haltInstruction, status := rs }

def CsrCause.illegalInstruction : CsrCause :=
  { kind := .illegalInstruction, status := 0b00000000#8 }

def CsrCause.privilegedInstruction : CsrCause :=
  { kind := .privilegedInstruction, status := 0b00000000#8 }

def CsrCause.addressOverflow (ovf : OverflowKind) : CsrCause :=
  { kind := .addressOverflow, status := ovf.enc }

def CsrCause.pcOverflow : CsrCause :=
  { kind := .pcOverflow, status := 0b00000000#8 }

def CsrCause.privilegedAccess (acc : AccessKind) : CsrCause :=
  { kind := .privilegedAccess, status := acc.enc }

def CsrCause.vectorWrite (addr : AddrWord) : CsrCause :=
  { kind := .vectorWrite, status := addr.extractLsb' 0 4 ++ 0b0010#4 }

def CsrCause.invalidAccess (acc : AccessKind) : CsrCause :=
  { kind := .invalidAccess, status := acc.enc }

def CsrCause.unmappedAddress (acc : AccessKind) : CsrCause :=
  { kind := .unmappedAddress, status := acc.enc }

def CsrCause.improperReturnContext (inSyscall inInterrupt : Bool) (csr : CsrReg) : CsrCause :=
  { kind   := .improperReturnContext,
    status := csr.enc ++ 0b00#2 ++ BitVec.ofBool inInterrupt ++ BitVec.ofBool inSyscall }

def CsrCause.conditionalContextReturn (cmp : CmpFlag) (csr : CsrReg) : CsrCause :=
  { kind := .conditionalContextReturn, status := csr.enc ++ 0b0#1 ++ cmp.enc }

def CsrCause.improperSystemCall (inSyscall inInterrupt : Bool) : CsrCause :=
  { kind   := .improperSystemCall,
    status := 0b000000#6 ++ BitVec.ofBool inInterrupt ++ BitVec.ofBool inSyscall }


end MisaSpec

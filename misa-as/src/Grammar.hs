module Grammar (Program,
                Stat(..),
                Inst(..),
                Op(..),
                GpReg(..),
                WideReg(..),
                CsrReg(..),
                CmpFlag(..),
                Imm(..),
                ImmPart(..),
                Label,
                Dir(..)) where


import Data.Word (Word8, Word16)


type Program = [Stat]


data Stat
  = InstStat Inst
  | LabelStat Label
  | DirStat Dir
  deriving (Show, Eq)


data Inst
  -- Base instructions
  = HaltInst GpReg
  | AddInst  GpReg   GpReg GpReg
  | AdcInst  GpReg   GpReg GpReg
  | SubInst  GpReg   GpReg GpReg
  | SbbInst  GpReg   GpReg GpReg
  | AndInst  GpReg   GpReg GpReg
  | OrInst   GpReg   GpReg GpReg
  | XorInst  GpReg   GpReg GpReg
  | RrcInst  GpReg   GpReg
  | SetInst  GpReg   Imm
  | LdInst   GpReg   GpReg GpReg
  | StInst   GpReg   GpReg GpReg
  | RsrInst  CsrReg  GpReg GpReg
  | WsrInst  CsrReg  GpReg GpReg
  | JalInst  CmpFlag GpReg GpReg
  | JmpInst  CmpFlag GpReg GpReg
  -- Pseudo instructions
  | Add2Inst  GpReg   GpReg GpReg GpReg GpReg GpReg
  | And2Inst  GpReg   GpReg GpReg GpReg GpReg GpReg
  | CallInst  Imm
  | ClrInst
  | CmpInst   GpReg   GpReg
  | GotoInst  Imm
  | JaliInst  CmpFlag Imm
  | JmpiInst  CmpFlag Imm
  | Ld2Inst   GpReg   GpReg GpReg GpReg
  | MovInst   GpReg   GpReg
  | Mov2Inst  GpReg   GpReg GpReg GpReg
  | NopInst
  | Or2Inst   GpReg   GpReg GpReg GpReg GpReg GpReg
  | PopInst   GpReg
  | Pop2Inst  GpReg   GpReg
  | PushInst  GpReg
  | Push2Inst GpReg   GpReg
  | RetInst
  | Rrc2Inst  GpReg   GpReg GpReg GpReg
  | Set2Inst  GpReg   GpReg Imm
  | St2Inst   GpReg   GpReg GpReg GpReg
  | Sub2Inst  GpReg   GpReg GpReg GpReg GpReg GpReg
  | Xor2Inst  GpReg   GpReg GpReg GpReg GpReg GpReg
  deriving (Show, Eq)


data Op
  -- Base instructions
  = HALT | ADD | ADC | SUB | SBB | AND | OR | XOR | RRC | SET | LD | ST | RSR | WSR | JAL | JMP
  -- Pseudo instructions
  | ADD2 | AND2 | CALL | CLR | CMP | GOTO | JALI | JMPI | MOV | MOV2 | NOP | OR2 | POP | POP2
  | PUSH | PUSH2 | RET | RRC2 | SET2 | SUB2 | XOR2
  deriving (Show, Enum, Bounded)


data GpReg
  = R0 | RA | RB | RC | RD | RE | RF | RU | RV | RW | RX | RY | RZ | RT
  | RSCRATCH0 | RSCRATCH1
  deriving (Show, Enum, Bounded, Eq)


data WideReg = RAB | RCD | REF | RUV | RWX | RYZ | RSCRATCH
  deriving (Show, Enum, Bounded)


data CsrReg = SADDR | RADDR | FLAGS | CAUSE
  deriving (Show, Enum, Bounded, Eq)


data CmpFlag = ALWAYS | EQUAL | NOT_EQUAL | GREATER | LESS | GREATER_EQUAL | LESS_EQUAL
  deriving (Show, Enum, Bounded, Eq)


data Imm
  = IntImm ImmPart Int
  | LabelImm ImmPart Label
  deriving (Show, Eq)


data ImmPart
  = Low
  | High
  | Full
  deriving (Show, Eq)


type Label = String


data Dir
  = WordDir Word8
  | ArrayDir [Word8]
  | AddrDir Label
  | AsciiDir String
  | AsciizDir String
  | SpaceDir Word16
  | SectionDir String
  deriving (Show, Eq)

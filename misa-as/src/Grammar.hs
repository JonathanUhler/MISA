{- |
Grammar definition for the MISA assembler language.

The grammar is context-free and defined by the production rules outlines with the `data` types
in this file, where the start variable is `Program`.

Author: Jonathan Uhler
-}
module Grammar (Program,
                Statement(..),
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


import Data.Word (Word8)


-- | The start symbol for the grammar of a full program (the contents of an assembly file).
type Program = [Statement]


-- | The type of every line of an assembly program (the most broad classification of lexemes).
data Statement
  -- | A statement which is an instruction containing an opcode and operands.
  = InstStatement Inst
  -- | A statement which is a label declaration.
  | LabelStatement Label
  -- | A statement which is a reserved assembler or linker directive.
  | DirStatement Dir
  deriving (Show, Eq)


-- | The type of an instruction in the assembly language.
data Inst
  -- Base instructions
  = AddInst  GpReg   GpReg GpReg
  | AdcInst  GpReg   GpReg GpReg
  | SubInst  GpReg   GpReg GpReg
  | SbbInst  GpReg   GpReg GpReg
  | AndInst  GpReg   GpReg GpReg
  | OrInst   GpReg   GpReg GpReg
  | XorInst  GpReg   GpReg GpReg
  | RrcInst  GpReg   GpReg
  | LwInst   GpReg   GpReg GpReg
  | SwInst   GpReg   GpReg GpReg
  | RsrInst  CsrReg  GpReg GpReg
  | WsrInst  CsrReg  GpReg GpReg
  | SetInst  GpReg   Imm
  | JalInst  CmpFlag GpReg GpReg
  | JmpInst  CmpFlag GpReg GpReg
  | HaltInst GpReg
  -- Pseudo instructions
  | NopInst
  | MovInst  GpReg   GpReg
  | CmpInst  GpReg   GpReg
  | Set2Inst GpReg   GpReg Imm
  | JmpiInst CmpFlag Imm
  | JaliInst CmpFlag Imm
  | GotoInst Imm
  | CallInst Imm
  | RetInst
  | ClrInst
  deriving (Show, Eq)


-- | The list of instruction opcodes.
data Op
  -- Base instructions
  = ADD | ADC | SUB | SBB | AND | OR | XOR | RRC | LW | SW | RSR | WSR | SET | JAL | JMP | HALT
  -- Pseudo instructions
  | NOP | MOV | CMP | SET2 | JMPI | JALI | GOTO | CALL | RET | CLR
  deriving (Show, Enum, Bounded)


-- | The list of general purpose register names.
data GpReg
  = R0 | RA | RB | RC | RD | RE | RF | RU | RV | RW | RX | RY | RZ | RT
  | RSCRATCH0 | RSCRATCH1
  deriving (Show, Enum, Bounded, Eq)


-- | The list of 16-bit general purpose register aliases.
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


-- | The type of a label definition/name.
type Label = String


-- | The type of an assembler or linker directive.
data Dir
  -- | The .word directive, .word X => produce byte X in output
  = WordDir Word8
  -- | The .array directive, .array X1 X2 ... => produce bytes X1 X2 ... in output
  | ArrayDir [Word8]
  -- | The .section directive, .section NAME => place following binary in section called NAME
  | SectionDir String
  deriving (Show, Eq)

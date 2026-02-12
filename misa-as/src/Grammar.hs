{- |
Grammar definition for the MISA assembler language.

The grammar is context-free and defined by the production rules outlines with the `data` types
in this file, where the start variable is `Program`.

Author: Jonathan Uhler
-}
module Grammar (Program,
                Statement(..),
                Instruction(..),
                Label,
                Directive(..),
                Opcode(..),
                Register(..),
                Immediate(..),
                ImmediatePart(..)) where


import Data.Word (Word8)


-- | The start symbol for the grammar of a full program (the contents of an assembly file).
type Program = [Statement]


-- | The type of every line of an assembly program (the most broad classification of lexemes).
data Statement
  -- | A statement which is an instruction containing an opcode and operands.
  = InstructionStatement Instruction
  -- | A statement which is a label declaration.
  | LabelStatement Label
  -- | A statement which is a reserved assembler or linker directive.
  | DirectiveStatement Directive
  deriving (Show, Eq, Ord)


-- | The type of an instruction in the assembly language.
data Instruction
  -- Base instructions
  -- | The add instruction, ADD RD RS1 RS2 => RD = RS1 + RS2
  = AddInstruction Register Register Register
  -- | The add with carry instruction, ADC RD RS1 RS2 => RD = RS1 + RS2 + CarryOut
  | AdcInstruction Register Register Register
  -- | The subtract instruction, SUB RD RS1 RS2 => RD = RS1 - RS2
  | SubInstruction Register Register Register
  -- | The bitwise and instruction, AND RD RS1 RS2 => RD = RS1 & RS2
  | AndInstruction Register Register Register
  -- | The bitwise or instruction, OR RD RS1 RS2 => RD = RS1 | RS2
  | OrInstruction Register Register Register
  -- | The bitwise xor instruction, XOR RD RS1 RS2 => RD = RS1 ^ RS2
  | XorInstruction Register Register Register
  -- | The load word instruction, LW RD IMM => RD = Memory[AR + IMM]
  | LwInstruction Register Immediate
  -- | The store word instruction, SW RD IMM => Memory[AR + IMM] = RD
  | SwInstruction Register Immediate
  -- | The load from address register instruction, LA RS1 RS2 => RS1 = AR[15:8]; RS2 = AR[7:0]
  | LaInstruction Register Register
  -- | The store to address register instruction, SA RS1 RS2 => AR[15:8] = RS1; AR[7:0] = RS2
  | SaInstruction Register Register
  -- | The load immediate instruction, LI RD IMM => RD = IMM
  | LiInstruction Register Immediate
  -- | The jump and link if zero instruction, JLZ RD IMM => if !(RD), {AR = PC + 2; PC = AR + IMM}
  | JlzInstruction Register Immediate
  -- | The jump if zero instruction, JZ RD IMM => if !(RD), {PC = AR + IMM}
  | JzInstruction Register Immediate
  -- | The halt processor instruction, HALT IMM => exit(IMM)
  | HaltInstruction Immediate
  -- Pseudo instructions
  -- | The bitwise not pseudo instruction, NOT RD RS1 => RD = ~RS1
  | NotInstruction Register Register
  -- | The move pseudo instruction, MOV RD RS1 => RD = RS1
  | MovInstruction Register Register
  -- | The 16-bit load immediate pseudo instruction, LDI RS1 RS2 IMM => {RS1, RS2} = IMM
  | LdiInstruction Register Register Immediate
  -- | The function call pseudo instruction, CALL IMM => {RAHI, RALO} = AR = IMM; JLZ ZERO 0
  | CallInstruction Immediate
  -- | The function entry pseudo instruction, ENTRY => LA RAHI RALO
  | EntryInstruction
  -- | The function return pseudo instruction, RET => SA RAHI RALO; JZ ZERO 0
  | RetInstruction
  -- | The stack push pseudo instruction, PUSH RS1 => SA SPHI SPLO; SW RS1 0; SUB {SPHI, SPLO} 1
  | PushInstruction Register
  -- | The stack pop pseudo instruction, POP RS1 => ADD {SPHI, SPLO} 1; SA SPHI SPLO; LW RS1 0
  | PopInstruction Register
  deriving (Show, Eq, Ord)


-- | The type of a label definition/name.
type Label = String


-- | The type of an assembler or linker directive.
data Directive
  -- | The .word directive, .word X => produce byte X in output
  = WordDirective    Word8
  -- | The .array directive, .array X1 X2 ... => produce bytes X1 X2 ... in output
  | ArrayDirective   [Word8]
  -- | The .section directive, .section NAME => place following binary in section called NAME
  | SectionDirective String
  deriving (Show, Eq, Ord)


-- | The list of instruction opcodes.
data Opcode
  -- Base instructions
  = ADD
  | ADC
  | SUB
  | AND
  | OR
  | XOR
  | LW
  | SW
  | LA
  | SA
  | LI
  | JLZ
  | JZ
  | HALT
  -- Pseudo instructions
  | NOT
  | MOV
  | LDI
  | CALL
  | ENTRY
  | RET
  | PUSH
  | POP
  deriving (Show, Enum, Eq, Ord)


-- | The list of general purpose register names.
data Register
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Show, Enum, Eq, Ord)


data Immediate
  = IntImmediate ImmediatePart Int
  | LabelImmediate ImmediatePart Label
  deriving (Show, Eq, Ord)


data ImmediatePart
  = Low
  | High
  | Full
  deriving (Show, Eq, Ord)

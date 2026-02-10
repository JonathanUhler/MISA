module Grammar (Program,
                Statement(..),
                Instruction(..),
                Label,
                Directive(..),
                Opcode(..),
                Register(..),
                minRegister,
                maxRegister) where


import Data.Int (Int8)


type Program = [Statement]


data Statement
  = InstructionStatement Instruction
  | LabelStatement Label
  | DirectiveStatement Directive
  deriving Show


data Instruction
  = AddInstruction  Register Register Register
  | AdcInstruction  Register Register Register
  | SubInstruction  Register Register Register
  | AndInstruction  Register Register Register
  | OrInstruction   Register Register Register
  | XorInstruction  Register Register Register
  | LwInstruction   Register Int8
  | SwInstruction   Register Int8
  | LaInstruction   Register Register
  | SaInstruction   Register Register
  | LiInstruction   Register Int8
  | JlzInstruction  Register Int8
  | HaltInstruction Int8
  deriving Show


type Label = String


data Directive
  = WordDirective    Int8
  | ArrayDirective   [Int8]
  | SectionDirective String
  deriving Show


data Opcode
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
  | HALT
  deriving (Show, Enum, Eq)


minRegister :: Int
minRegister = 0

maxRegister :: Int
maxRegister = 15


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
  deriving (Show, Enum, Eq)

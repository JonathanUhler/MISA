module Grammar (Opcode(..), minRegister, maxRegister, Register(..), Directive(..)) where


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


data Directive
  = WordDirective
  | ArrayDirective
  | SectionDirective
  deriving (Show, Enum, Eq)

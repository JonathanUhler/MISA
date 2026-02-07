module Architecture (Register(..), Instruction(..)) where


import Data.Word (Word8)


data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show, Enum)


data Instruction
  = ADD  Register Register Register
  | ADC  Register Register Register
  | SUB  Register Register Register
  | AND  Register Register Register
  | OR   Register Register Register
  | XOR  Register Register Register
  | LW   Register
  | SW   Register
  | LA   Register Register
  | SA   Register Register
  | LI   Register Word8
  | JLZ  Register Word8
  | HALT Word8
  deriving Show

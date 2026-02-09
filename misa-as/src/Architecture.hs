module Architecture (Register(..), Instruction(..), minRegister, maxRegister) where


import Data.Word (Word8)


minRegister :: Int
minRegister = 0

maxRegister :: Int
maxRegister = 15


data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show, Enum)


data Instruction
  = ADD  Register Register Register
  | ADC  Register Register Register
  | SUB  Register Register Register
  | AND  Register Register Register
  | OR   Register Register Register
  | XOR  Register Register Register
  | LW   Register Word8
  | SW   Register Word8
  | LA   Register Register
  | SA   Register Register
  | LI   Register Word8
  | JLZ  Register Word8
  | HALT Word8
  deriving Show

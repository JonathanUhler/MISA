module Encoder (encodeProgram) where


import Architecture

import Data.Bits (shiftL, (.|.), (.&.))
import Data.Word (Word8)


encodeRegister :: Register -> Word8
encodeRegister register = fromIntegral (fromEnum register) .&. 0x0F


encodeFormat1Instruction :: Word8 -> Register -> Register -> Register -> (Word8, Word8)
encodeFormat1Instruction opcode rd rs1 rs2 =
  (opcode .|. shiftL (encodeRegister rd) 4, encodeRegister rs1 .|. shiftL (encodeRegister rs2) 4)


encodeFormat2Instruction :: Word8 -> Register -> Word8 -> (Word8, Word8)
encodeFormat2Instruction opcode rd imm =
  (opcode .|. shiftL (encodeRegister rd) 4, imm)


encodeFormat3Instruction :: Word8 -> Register -> Register -> (Word8, Word8)
encodeFormat3Instruction opcode rs1 rs2 =
  (opcode, encodeRegister rs1 .|. shiftL (encodeRegister rs2) 4)


encodeInstruction :: Instruction -> (Word8, Word8)
encodeInstruction (ADD  rd  rs1 rs2) = encodeFormat1Instruction 0x0 rd  rs1  rs2
encodeInstruction (ADC  rd  rs1 rs2) = encodeFormat1Instruction 0x1 rd  rs1  rs2
encodeInstruction (SUB  rd  rs1 rs2) = encodeFormat1Instruction 0x2 rd  rs1  rs2
encodeInstruction (AND  rd  rs1 rs2) = encodeFormat1Instruction 0x3 rd  rs1  rs2
encodeInstruction (OR   rd  rs1 rs2) = encodeFormat1Instruction 0x4 rd  rs1  rs2
encodeInstruction (XOR  rd  rs1 rs2) = encodeFormat1Instruction 0x5 rd  rs1  rs2
encodeInstruction (LW   rd  imm)     = encodeFormat2Instruction 0x8 rd  imm
encodeInstruction (SW   rd  imm)     = encodeFormat2Instruction 0x9 rd  imm
encodeInstruction (LA   rs1 rs2)     = encodeFormat3Instruction 0xA rs1 rs2
encodeInstruction (SA   rs1 rs2)     = encodeFormat3Instruction 0xB rs1 rs2
encodeInstruction (LI   rd  imm)     = encodeFormat2Instruction 0xC rd  imm
encodeInstruction (JLZ  rd  imm)     = encodeFormat2Instruction 0xD rd  imm
encodeInstruction (HALT imm)         = encodeFormat2Instruction 0xF R0  imm


encodeProgram :: [Instruction] -> [Word8]
encodeProgram [] = []
encodeProgram (i : is) = [lowByte, highByte] ++ encodeProgram is
  where (lowByte, highByte) = encodeInstruction i

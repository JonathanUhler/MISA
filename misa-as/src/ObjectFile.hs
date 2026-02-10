module ObjectFile (BinaryObject,
                   Section(..),
                   Code,
                   SymbolTable,
                   Symbol(..),
                   RelocationTable,
                   Relocation(..),
                   packBinaryObject) where


import Grammar

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word (Word8, Word16)


type BinaryObject = [Section]


data Section = Section Label Code SymbolTable RelocationTable
  deriving Show


type Code = [Instruction]


type SymbolTable = [Symbol]


data Symbol = Symbol Label Word16
  deriving Show


type RelocationTable = [Relocation]


data Relocation = Relocation RelocationType Word16 Label
  deriving Show


data RelocationType = LowRelocation | HighRelocation
  deriving (Show, Enum)


packString :: String -> [Word8]
packString string
  = packDoubleWord (fromIntegral (length packedString)) ++ packedString
  where packedString = B.unpack (E.encodeUtf8 (T.pack string))


packDoubleWord :: Word16 -> [Word8]
packDoubleWord dword = [fromIntegral (dword .&. 0x00FF), fromIntegral (shiftR dword 8)]


packInstruction :: Instruction -> [Word8]
packInstruction (AddInstruction rd rs1 rs2)
  = [0x0 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (AdcInstruction rd rs1 rs2)
  = [0x1 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (SubInstruction rd rs1 rs2)
  = [0x2 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (AndInstruction rd rs1 rs2)
  = [0x3 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (OrInstruction rd rs1 rs2)
  = [0x4 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (XorInstruction rd rs1 rs2)
  = [0x5 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (LwInstruction rd imm)
  = [0x8 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     imm]
packInstruction (SwInstruction rd imm)
  = [0x9 .|. shiftL (fromIntegral (fromEnum rd)) 4,
     imm]
packInstruction (LaInstruction rs1 rs2)
  = [0xA,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (SaInstruction rs1 rs2)
  = [0xB,
     fromIntegral (fromEnum rs1) .|. shiftL (fromIntegral (fromEnum rs2)) 4]
packInstruction (LiInstruction rd imm)
  = [0xC .|. shiftL (fromIntegral (fromEnum rd)) 4,
     imm]
packInstruction (JlzInstruction rd imm)
  = [0xD .|. shiftL (fromIntegral (fromEnum rd)) 4,
     imm]
packInstruction (HaltInstruction imm)
  = [0xF,
     imm]


packCode :: Code -> [Word8]
packCode code
  = packDoubleWord (fromIntegral (length packedInstructions)) ++ packedInstructions
  where packedInstructions = concatMap packInstruction code


packSymbols :: SymbolTable -> [Word8]
packSymbols [] = packDoubleWord 0
packSymbols symbols
  = packDoubleWord (fromIntegral (length symbols)) ++ packedSymbols
  where packedSymbols = concatMap packSymbol symbols


packSymbol :: Symbol -> [Word8]
packSymbol (Symbol name address)
  = packDoubleWord address ++ packString name


packRelocations :: RelocationTable -> [Word8]
packRelocations [] = packDoubleWord 0
packRelocations relocations
  = packDoubleWord (fromIntegral (length relocations)) ++ packedRelocations
  where packedRelocations = concatMap packRelocation relocations


packRelocation :: Relocation -> [Word8]
packRelocation (Relocation kind address name)
  =  [fromIntegral (fromEnum kind)]
  ++ packDoubleWord address
  ++ packString name


packSection :: Section -> [Word8]
packSection (Section name code symbols relocations)
  =  packString name
  ++ packCode code
  ++ packSymbols symbols
  ++ packRelocations relocations
  

packBinaryObject :: BinaryObject -> [Word8]
packBinaryObject [] = packDoubleWord 0
packBinaryObject sections
  = packDoubleWord (fromIntegral (length sections)) ++ packedSections
  where packedSections = concatMap packSection sections

{- |
A basic object file containing linker sections with code, symbols, and relocations.

Author: Jonathan Uhler
-}
module ObjectFile (BinaryObject,
                   Section(..),
                   Code,
                   CodeElement(..),
                   SymbolTable,
                   Symbol(..),
                   RelocationTable,
                   Relocation(..),
                   RelocationType(..),
                   packBinaryObject) where


import Grammar

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word (Word8, Word16)


-- | The type of an object file, which is a list of zero or more sections.
type BinaryObject = [Section]


{- |
The description of a linker section, which is a named tuple of code (binary data to emit in the
object file/final binary), symbols defined within the section and their section-relative addresses,
and relocations used in the section with section-relative addresses to the "holes" in the code.
-}
data Section = Section Label Code SymbolTable RelocationTable
  deriving (Show, Eq)


-- | The type of a section's code, which is a list of instructions or binary literals.
type Code = [CodeElement]


-- | The type of a single unit in a section's code.
data CodeElement
  -- | An assembly instruction in the code of a section.
  = InstructionCode Instruction
  -- | Literal binary data in the code of a section (e.g. from .array/.word directives).
  | LiteralCode [Word8]
  deriving (Show, Eq)


-- | The type of a symbol table.
type SymbolTable = [Symbol]


-- | The type of a symbol in a section, which is a labeled section-relative address.
data Symbol = Symbol Label Word16
  deriving (Show, Eq)


-- | The type of a relocation table.
type RelocationTable = [Relocation]


{- |
The type of a relocation in a section, which points to a section-relative address where a "hole"/
missing symbol existed in the assembly code. The relocation includes the name of the missing symbol
and which part of the symbol (e.g. high or low word) should be used to fill the hole.
-}
data Relocation = Relocation RelocationType Word16 Label
  deriving (Show, Eq)


-- | How to fill a relocation.
data RelocationType
  -- | Use the lower half of the 16-bit word referred to by the relocation symbol to fill the hole.
  = LowRelocation
  -- | Use the upper half of the 16-bit word referred to by the relocation symbol to fill the hole.
  | HighRelocation
  deriving (Show, Enum, Eq)


-- | A magic 8-byte string that appears at the start of every object file in little-endian order.
magicHeader :: String
magicHeader = "MISA_LF "


{- |
Packs an ASCII string into a list of bytes.

The returned list begins with a 2-byte doubleword in little-endian format which is the number of
bytes in the following string. The list then contains all the bytes of the string using UTF8
encoding and little-endian order (first character of the string at lowest address).
-}
packString :: String -> [Word8]
packString string
  = packDoubleWord (fromIntegral (length packedString)) ++ packedString
  where packedString = B.unpack (E.encodeUtf8 (T.pack string))


{- |
Packs a 16-bit word into two 8-bit words.

The returned list is in little-endian format.
-}
packDoubleWord :: Word16 -> [Word8]
packDoubleWord dword = [fromIntegral (dword .&. 0x00FF), fromIntegral (shiftR dword 8)]


{- |
Packs an assembly instruction into bytes.

The returned list is one or more instructions (2 bytes each in the ISA) in little-endian order.
All base instructions will be 2-byte lists, and some pseudo-instructions may expand to more than
one base instruction.
-}
packInstruction :: Instruction -> [Word8]
packInstruction instruction = case instruction of
  AddInstruction rd rs1 rs2 -> packRRR 0x0 rd rs1 rs2
  AdcInstruction rd rs1 rs2 -> packRRR 0x1 rd rs1 rs2
  SubInstruction rd rs1 rs2 -> packRRR 0x2 rd rs1 rs2
  AndInstruction rd rs1 rs2 -> packRRR 0x3 rd rs1 rs2
  OrInstruction rd rs1 rs2  -> packRRR 0x4 rd rs1 rs2
  XorInstruction rd rs1 rs2 -> packRRR 0x5 rd rs1 rs2
  LwInstruction rd imm      -> packRI 0x8 rd imm
  SwInstruction rd imm      -> packRI 0x9 rd imm
  LaInstruction rs1 rs2     -> packRR 0xA rs1 rs2
  SaInstruction rs1 rs2     -> packRR 0xB rs1 rs2
  LiInstruction rd imm      -> packRI 0xC rd imm
  JlzInstruction rd imm     -> packRI 0xD rd imm
  JzInstruction rd imm      -> packRI 0xE rd imm
  HaltInstruction imm       -> packI 0xF imm
  where packRRR :: Word8 -> Register -> Register -> Register -> [Word8]
        packRRR op rd rs1 rs2 = [op .|. shiftL (fromR rd) 4, fromR rs1 .|. shiftL (fromR rs2) 4]

        packRI :: Word8 -> Register -> Immediate -> [Word8]
        packRI op rd imm = [op .|. shiftL (fromR rd) 4, fromI imm]

        packRR :: Word8 -> Register -> Register-> [Word8]
        packRR op rs1 rs2 = [op, fromR rs1 .|. shiftL (fromR rs2) 4]

        packI :: Word8 -> Immediate -> [Word8]
        packI op imm = [op, fromI imm]

        fromI :: Immediate -> Word8
        fromI (IntImmediate n) = fromIntegral n
        fromI (LabelImmediate _) = 0x00

        fromR :: Register -> Word8
        fromR r = fromIntegral (fromEnum r)


{- |
Packs the code portion of a section into bytes.

The returned list of bytes begins with a 2-byte doubleword in little-endian order which is
the number of bytes of the packed code, followed by the packed code.

Note that interpreting the code once it's packed will be difficult. The linker only needs to
know what bytes in the packed code correspond to relocations, but not which parts of the code
are instructions vs. literal binary data.
-}
packCode :: Code -> [Word8]
packCode code
  = packDoubleWord (fromIntegral (length packedCode)) ++ packedCode
  where packedCode = concatMap packCodeElement code


{- |
Packs a single code element into binary.

If the code element is an instruction, `packInstruction` is used, otherwise the literal binary
byte array is returned.
-}
packCodeElement :: CodeElement -> [Word8]
packCodeElement (InstructionCode instruction) = packInstruction instruction
packCodeElement (LiteralCode bytes) = bytes


{- |
Packs a symbol table into binary.

The returned list begins with a 2-byte doubleword in little-endian order which is the number of
packed symbols present in the following binary. The list is followed by all of the packed
symbols in order.

Note carefully that the length field is the number of packed symbols, not the total number of
bytes of all packed symbols. Thus to skip past the packed symbol table, each symbol's size
(see `packSymbol`) must be read and skipped in O(n) time.
-}
packSymbols :: SymbolTable -> [Word8]
packSymbols [] = packDoubleWord 0
packSymbols symbols
  = packDoubleWord (fromIntegral (length symbols)) ++ packedSymbols
  where packedSymbols = concatMap packSymbol symbols


{- |
Packs a single symbol into binary.

The returned list begins with the address of the symbol as a 2-byte little-endian doubleword,
followed by the packed name of the symbol (which starts with the size of the string).
-}
packSymbol :: Symbol -> [Word8]
packSymbol (Symbol name address)
  = packDoubleWord address ++ packString name


{- |
Packs a relocation table into binary.

The returned list begins with a 2-byte doubleword in little-endian order that is the number of
relocations in the following binary, followed by all the packed relocations.

Note carefully that the length field is the number of relocations, not the number of bytes in
the packed relocations.
-}
packRelocations :: RelocationTable -> [Word8]
packRelocations [] = packDoubleWord 0
packRelocations relocations
  = packDoubleWord (fromIntegral (length relocations)) ++ packedRelocations
  where packedRelocations = concatMap packRelocation relocations


{- |
Packs a single relocation into binary.

The returned list begins with the address in the code of the byte requiring relocation/resolution
as a 2-byte doubleword, followed by the packed name (which begins with the length of the name
string).
-}
packRelocation :: Relocation -> [Word8]
packRelocation (Relocation kind address name)
  =  [fromIntegral (fromEnum kind)]
  ++ packDoubleWord address
  ++ packString name


{- |
Packs a single section into binary.

The returned list is the packed sectoin name, packed code, packed symbols, and packed relocation.
See other functions for more information on the format.
-}
packSection :: Section -> [Word8]
packSection (Section name code symbols relocations)
  =  packString name
  ++ packCode code
  ++ packSymbols symbols
  ++ packRelocations relocations
  

{- |
Packs a binary object into a byte array.

The returned byte array begins with the magic 8-byte header `magicHeader`, followed by a 2-byte
doubleword in little-endian order which is the number of packed sections that follow, followed
finally by all the packed sections.

Note carefully that the length field it the number of sections, not the number of bytes of all
sections. To navigate the object file, sub-components must be recursively visited (e.g. check
the size of a section's code, number of symbols, size of each symbol, number of relocations, ...)
-}
packBinaryObject :: BinaryObject -> [Word8]
packBinaryObject [] = packDoubleWord 0
packBinaryObject sections
  =  B.unpack (E.encodeUtf8 (T.pack magicHeader))
  ++ packDoubleWord (fromIntegral (length sections))
  ++ packedSections
  where packedSections = concatMap packSection sections

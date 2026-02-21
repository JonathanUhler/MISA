{- |
A basic object file containing linker sections with code, symbols, and relocations.

Author: Jonathan Uhler
-}
module ObjectFile (BinaryObject,
                   Sec(..),
                   Code,
                   CodeElem(..),
                   SymTable,
                   Sym(..),
                   RelocTable,
                   Reloc(..),
                   RelocType(..),
                   packBinaryObject) where


import Grammar

import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word (Word8, Word16)


-- | The type of an object file, which is a list of zero or more sections.
type BinaryObject = [Sec]


{- |
The description of a linker section, which is a named tuple of code (binary data to emit in the
object file/final binary), symbols defined within the section and their section-relative addresses,
and relocations used in the section with section-relative addresses to the "holes" in the code.
-}
data Sec = Sec Label Code SymTable RelocTable
  deriving (Show)


-- | The type of a section's code, which is a list of instructions or binary literals.
type Code = [CodeElem]


-- | The type of a single unit in a section's code.
data CodeElem
  -- | An assembly instruction in the code of a section.
  = InstCode Inst
  -- | Literal binary data in the code of a section (e.g. from .array/.word directives).
  | LiteralCode [Word8]
  deriving (Show)


-- | The type of a symbol table.
type SymTable = [Sym]


-- | The type of a symbol in a section, which is a labeled section-relative address.
data Sym = Sym Label Word16
  deriving (Show)


-- | The type of a relocation table.
type RelocTable = [Reloc]


{- |
The type of a relocation in a section, which points to a section-relative address where a "hole"/
missing symbol existed in the assembly code. The relocation includes the name of the missing symbol
and which part of the symbol (e.g. high or low word) should be used to fill the hole.
-}
data Reloc = Reloc RelocType Word16 Label
  deriving (Show)


-- | How to fill a relocation.
data RelocType
  -- | Use the lower half of the 16-bit word referred to by the relocation symbol to fill the hole.
  = LowReloc
  -- | Use the upper half of the 16-bit word referred to by the relocation symbol to fill the hole.
  | HighReloc
  deriving (Show, Enum)


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
packInst :: Inst -> [Word8]
packInst inst =
  case inst of
    HaltInst rs1          -> packFormatR   0x0 rs1
    AddInst  rd   rs1 rs2 -> packFormatRRR 0x1 rd   rs1 rs2
    AdcInst  rd   rs1 rs2 -> packFormatRRR 0x2 rd   rs1 rs2
    SubInst  rd   rs1 rs2 -> packFormatRRR 0x3 rd   rs1 rs2
    SbbInst  rd   rs1 rs2 -> packFormatRRR 0x4 rd   rs1 rs2
    AndInst  rd   rs1 rs2 -> packFormatRRR 0x5 rd   rs1 rs2
    OrInst   rd   rs1 rs2 -> packFormatRRR 0x6 rd   rs1 rs2
    XorInst  rd   rs1 rs2 -> packFormatRRR 0x7 rd   rs1 rs2
    RrcInst  rd   rs1     -> packFormatRR  0x8 rd   rs1
    SetInst  rd   imm     -> packFormatRI  0x9 rd   imm
    LwInst   rd   rs1 rs2 -> packFormatRRR 0xA rd   rs1 rs2
    SwInst   rd   rs1 rs2 -> packFormatRRR 0xB rd   rs1 rs2
    RsrInst  csr  rs1 rs2 -> packFormatCRR 0xC csr  rs1 rs2
    WsrInst  csr  rs1 rs2 -> packFormatCRR 0xD csr  rs1 rs2
    JalInst  flag rs1 rs2 -> packFormatFRR 0xE flag rs1 rs2
    JmpInst  flag rs1 rs2 -> packFormatFRR 0xF flag rs1 rs2
    _                     -> error ("cannot pack pseudo-instruction " ++ show inst)
  where
    packFormatR   op r1       = [op .|. shiftL (fromReg r1) 4, 0x00]
    packFormatRR  op r1 r2    = [op .|. shiftL (fromReg r1) 4, fromReg r2]
    packFormatRRR op r1 r2 r3 = [op .|. shiftL (fromReg r1) 4, fromReg r2 .|. shiftL (fromReg r3) 4]
    packFormatRI  op r  i     = [op .|. shiftL (fromReg r) 4,  fromImm i]
    packFormatCRR op c  r1 r2 = [op .|. shiftL (fromReg c) 4,  fromReg r1 .|. shiftL (fromReg r2) 4]
    packFormatFRR op f  r1 r2 = [op .|. shiftL (fromReg f) 4,  fromReg r1 .|. shiftL (fromReg r2) 4]
    fromReg r = fromIntegral (fromEnum r)
    fromImm i = case i of
      (IntImm Full n) -> fromIntegral n
      (IntImm Low n)  -> fromIntegral n
      (IntImm High n) -> fromIntegral (shiftR n 8)
      (LabelImm _ _)  -> 0x00


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
  where packedCode = concatMap packCodeElem code


{- |
Packs a single code element into binary.

If the code element is an instruction, `packInst` is used, otherwise the literal binary
byte array is returned.
-}
packCodeElem :: CodeElem -> [Word8]
packCodeElem (InstCode inst) = packInst inst
packCodeElem (LiteralCode bytes)    = bytes


{- |
Packs a symbol table into binary.

The returned list begins with a 2-byte doubleword in little-endian order which is the number of
packed symbols present in the following binary. The list is followed by all of the packed
symbols in order.

Note carefully that the length field is the number of packed symbols, not the total number of
bytes of all packed symbols. Thus to skip past the packed symbol table, each symbol's size
(see `packSym`) must be read and skipped in O(n) time.
-}
packSyms :: SymTable -> [Word8]
packSyms [] = packDoubleWord 0
packSyms symbols
  = packDoubleWord (fromIntegral (length symbols)) ++ packedSyms
  where packedSyms = concatMap packSym symbols


{- |
Packs a single symbol into binary.

The returned list begins with the address of the symbol as a 2-byte little-endian doubleword,
followed by the packed name of the symbol (which starts with the size of the string).
-}
packSym :: Sym -> [Word8]
packSym (Sym name address)
  = packDoubleWord address ++ packString name


{- |
Packs a relocation table into binary.

The returned list begins with a 2-byte doubleword in little-endian order that is the number of
relocations in the following binary, followed by all the packed relocations.

Note carefully that the length field is the number of relocations, not the number of bytes in
the packed relocations.
-}
packRelocs :: RelocTable -> [Word8]
packRelocs [] = packDoubleWord 0
packRelocs relocations
  = packDoubleWord (fromIntegral (length relocations)) ++ packedRelocs
  where packedRelocs = concatMap packReloc relocations


{- |
Packs a single relocation into binary.

The returned list begins with the address in the code of the byte requiring relocation/resolution
as a 2-byte doubleword, followed by the packed name (which begins with the length of the name
string).
-}
packReloc :: Reloc -> [Word8]
packReloc (Reloc kind address name)
  =  [fromIntegral (fromEnum kind)]
  ++ packDoubleWord address
  ++ packString name


{- |
Packs a single section into binary.

The returned list is the packed sectoin name, packed code, packed symbols, and packed relocation.
See other functions for more information on the format.
-}
packSec :: Sec -> [Word8]
packSec (Sec name code symbols relocations)
  =  packString name
  ++ packCode code
  ++ packSyms symbols
  ++ packRelocs relocations
  

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
  ++ packedSecs
  where packedSecs = concatMap packSec sections

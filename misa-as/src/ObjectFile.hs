{- |
A basic object file containing linker sections with code, symbols, and relocations.

This object file format is referred to as the MISA linkable format (with the magic "MISA_LF " 8-byte
header, including the trailing space). It's purpose is to package code from a single translation
unit or source file into a semi-compiled format that can be consumed by the linker.

Object files in the MISA toolchain hold compiled/encoded source code, a list of symbols defined
in the translation unit, and a list of relocations (symbols that are required by but not defined in
the given translation unit). If relocations are required for an object file, the relocation address
("hole") in the code segment of the object file will be set to zeros.

For more information about the layout and encoding of MISA_LF object files, see the MISA ISA manual.

Author: Jonathan Uhler
-}
module ObjectFile (BinaryObject,
                   NamedBinaryObject(..),
                   Sec(..),
                   Code,
                   CodeElem(..),
                   SymTable,
                   Sym(..),
                   RelocTable,
                   Reloc(..),
                   RelocType(..),
                   packBinaryObject,
                   packNamedBinaryObject,
                   unpackBinaryObject,
                   unpackNamedBinaryObject,
                   getSecSize) where


import Grammar
import Packing

import Data.Bits ((.|.), shiftR, shiftL)
import qualified Data.ByteString as B
import Data.List (elemIndex, nub, union)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Void
import Data.Word (Word8, Word16)
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary


type Parser = Parsec Void B.ByteString


{- |
The type of a binary object file, which is simply a list of sections. Internally, object files
use the concept of a "string table" to compress string literals (e.g. for symbol and relocation
names), but string tables are expanded automatically by the unpack* functions and thus are never
seen externally when using BinaryObject.
-}
type BinaryObject = [Sec]


{- |
A container for attaching a string name to a BinaryObject. NamedBinaryObjects are primarily used
when creating archives (bunldes of one or more individual object files) so that the archived object
files can be extracted to the disk with their original filenames.
-}
data NamedBinaryObject = NamedBinaryObject Label BinaryObject


{- |
A section of a binary object, which is the core unit of data within object files. Each section comes
with the following information:

- A string name, which is set by the `.section` directive in the source assembly code.
- A code segment
- A symbol table
- A relocation table

Addresses for the symbol and relocation tables are always relative to the beginning of the section
in which the syms/relocs appear. It is the job of the linker to convert these to absolute addresses
after placing sections into memory.

Section names should be unique within one object file (although there is technically no pre-
condition that absolutely requires this), but the same section may appear across multiple object
files, which will be managed by the linker.
-}
data Sec = Sec Label Code SymTable RelocTable
  deriving (Show, Eq)


{- |
Internally, the ObjectFile module encodes symbol and relocation labels into one deduplicated table
of strings in binary object files. IndexSec is the internal section representation which uses
IndexSyms and IndexRelocs (containing indexes into the symbol table, rather than copies of the
string labels themselves).

This is primarily done to save space in the object files. However, because the extra layer of
indirection is annoying to work with in the assembler and linker code, the ObjectFile module
will decode IndexSecs and then automatically resolve the index --> label translations, returning
regular Secs to the caller.
-}
data IndexSec = IndexSec Label Code IndexSymTable IndexRelocTable


{- |
The code segment of a section, which is simply a list of code elements. The order of the CodeElems
matters: elements which are closer to the beginning of the Code array will be placed at lower
addresses in object files and memory after linking.

The Code array of a particular section may be empty, in which case no code will be emitted by the
linker for that section.
-}
type Code = [CodeElem]


{- |
CodeElems can represent one of two types of data:

1) InstCode, which comes from an assembly instruction in a source file. These CodeElems are only
   used by the assembler when packing code into object files. When object files are subsequently
   read (e.g. by the linker or misa-nm), their code will always be read as a single large
   LiteralCode element. Each InstCode element represents a single assembly instruction that will
   be packed into bytes when writing an object file.
2) LiteralCode, which represents arbitrary bytes. This types of CodeElems can come either from
   certain directives (e.g. .space, .array) or from reading a previously-created object file as
   mentioned previously.
-}
data CodeElem
  = InstCode Inst
  | LiteralCode [Word8]
  deriving (Show, Eq)


{- | The symbol table for a section, which is simply a list of symbols. -}
type SymTable = [Sym]


{- |
The indexed symbol table for an indexed section, which is simply a list of indexed symbols. See
the comment on IndexSec for the difference between regular and indexed sections.
-}
type IndexSymTable = [IndexSym]


{- |
A single symbol in a symbol table. Symbols are labels defined in a given translation unit which can
then be accessed by other translation units. During the linking phase, symbols are resolved to
the address of the first byte (e.g. instruction, .addr, .array) after the label. Each symbol
contains a string name (e.g. "loop_begin") and the address of the symbol relative to the beginning
of the code segment.

Once a section's code has been encoded to a single LiteralCode element, the symbol address is a
direct index into the LiteralCode byte-array. Before such encoding, locating the symbol requires
stepping through the Code array and counting the size of each InstCode/LiteralCode until the
symbol's relative address is reached.
-}
data Sym = Sym Label Word16
  deriving (Show, Eq)


{- |
A single indexed symbol in an indexed symbol table. See the comment on IndexSec for the difference
between regular and indexed sections.

Each indexed symbol contains a 0-aligned index into the string table (in place of the Label
parameter in regular Syms) and then the same relative address as regular Syms.
-}
data IndexSym = IndexSym Word16 Word16
  deriving (Show, Eq)


{- | The relocation table for a section, which is simply a list of relocations. -}
type RelocTable = [Reloc]

{- |
The indexed relocation table for an indexed section, which is simply a list of indexed relocs. See
the comment on IndexSec for the difference between regular and indexed sections.
-}
type IndexRelocTable = [IndexReloc]


{- |
A single relocation in a reloc table. Relocations are labels used within a translation unit. While
most relocations happen due to the usage of labels defined in external translation units, the
RelocTable for a section will contain all relocations (even for symbols defined in that section's
SymTable), just for simplicity.

Each relocation will generate a "hole" in the code segment, which is a placeholder byte (defaulting
to 0x00) that the linker overwrites with the address of the relocation's label. The Reloc datatype
contains the type of the relocation, the address of the "hole" byte relative to the start of the
code segment, and the name of the label whose address should be used to fill the hole.
-}
data Reloc = Reloc RelocType Word16 Label
  deriving (Show, Eq)


{- |
A single indexed relocation in an indexed relocation table. See the comment on IndexSec for the
difference between regular and indexed sections.

Each indexed relocation contains a 0-aligned index into the string table (in place of the Label
parameter in regular Relocs) and then the same reloc type and relative address as regular Relocs.
-}
data IndexReloc = IndexReloc RelocType Word16 Word16
  deriving (Show, Eq)


{- |
Since each Reloc represents a hole that is exactly one byte in size, multi-byte relocations (e.g.
using `set2 label`) require multiple relocations. The RelocType specifies which portion of the
multi-byte value to put into each of the holes.

A LowReloc will place the least-significant byte of a 16-bit doubleword into the hole, and a
HighReloc will place the most-significant byte of the dword into the hole. Relocations that are
truly only one byte should default to be LowRelocs. Relocations of values larger than 16-bits are
not supported by RelocType.
-}
data RelocType
  = LowReloc
  | HighReloc
  deriving (Show, Enum, Eq)


{- |
The type of a string table, which is simply a list of strings. The order matters very much for
the string table, as IndexSyms and IndexRelocs index into the string table starting from element 0.

String tables are used with Index* datatypes as a means of compressing string literals (e.g. labels)
in object files on the disk. The functions applyIndexes and resolveIndexes are responsible for
translating between Index* and regular datatypes using a StringTable. The general flow is:

- Create a BinaryObject with regular Secs
- Create a string table for the entire BinaryObject (all sections)
- Translate to IndexSecs and write those to the disk
- Load the IndexSecs back at a later time
- Read the string table and translate back to a BinaryObject with regular Secs
-}
type StringTable = [Label]


{- | Magic header bytes that are inserted as the first 8 bytes of every object file. -}
magicHeader :: String
magicHeader = "MISA_LF "


{- |
Converts a single core instruction to bytes. This function does not support encoding or translation
of pseudo-instructions (that must be handled by the Encoder module). This function does support
packing of core instructions in all architectural extensions however.

Each core instruction is fixed-width and will return a 2-byte array in little-endian order.
-}
packInst :: Inst -> [Word8]
packInst inst =
  case inst of
    HaltInst rs1          -> packFormatR   0x0 rs1
    AddInst  rd   rs1 rs2 -> packFormatRRR 0x1 rd  rs1 rs2
    AdcInst  rd   rs1 rs2 -> packFormatRRR 0x2 rd  rs1 rs2
    SubInst  rd   rs1 rs2 -> packFormatRRR 0x3 rd  rs1 rs2
    SbbInst  rd   rs1 rs2 -> packFormatRRR 0x4 rd  rs1 rs2
    AndInst  rd   rs1 rs2 -> packFormatRRR 0x5 rd  rs1 rs2
    OrInst   rd   rs1 rs2 -> packFormatRRR 0x6 rd  rs1 rs2
    XorInst  rd   rs1 rs2 -> packFormatRRR 0x7 rd  rs1 rs2
    RrcInst  rd   rs      -> packFormatRR  0x8 rd  rs
    SetInst  rd   imm     -> packFormatRI  0x9 rd  imm
    LdInst   rd   rs1 rs2 -> packFormatRRR 0xA rd  rs1 rs2
    StInst   rd   rs1 rs2 -> packFormatRRR 0xB rd  rs1 rs2
    RsrInst  csr  rs1 rs2 -> packFormatRRC 0xC rs1 rs2 csr
    WsrInst  csr  rs1 rs2 -> packFormatRRC 0xD rs1 rs2 csr
    JalInst  flag rs1 rs2 -> packFormatRRF 0xE rs1 rs2 flag
    JmpInst  flag rs1 rs2 -> packFormatRRF 0xF rs1 rs2 flag
    -- System call extension
    SyscallInst rs        -> [0x0 .|. shiftL (fromReg rs) 4, 0x80]
    _                     -> error ("cannot pack pseudo-instruction " ++ show inst)
  where
    packFormatR   op r1       = [op .|. shiftL (fromReg r1) 4, 0x00]
    packFormatRR  op r1 r2    = [op .|. shiftL (fromReg r1) 4, fromReg r2]
    packFormatRRR op r1 r2 r3 = [op .|. shiftL (fromReg r1) 4, fromReg r2 .|. shiftL (fromReg r3) 4]
    packFormatRI  op r  i     = [op .|. shiftL (fromReg r) 4,  fromImm i]
    packFormatRRC op r1 r2 c  = [op .|. shiftL (fromReg r1) 4, fromReg r2 .|. shiftL (fromCsr c) 4]
    packFormatRRF op r1 r2 f  = [op .|. shiftL (fromReg r1) 4, fromReg r2 .|. shiftL (fromFlag f) 4]
    fromReg r  = fromIntegral (fromEnum r)
    fromCsr c  = case c of
      SADDR -> 0x1
      RADDR -> 0x2
      FLAGS -> 0x3
      CAUSE -> 0x4
      EXTNS -> 0x5
      -- System call extension
      RETSC -> 0x8
      -- Privilege extension
      PRIVS -> 0xA
    fromFlag f = case f of
      ALWAYS        -> 0x0
      EQUAL         -> 0x1
      NOT_EQUAL     -> 0x8
      GREATER       -> 0x2
      LESS          -> 0x4
      GREATER_EQUAL -> 0x3
      LESS_EQUAL    -> 0x5
    fromImm i  = case i of
      (IntImm Full n) -> fromIntegral n
      (IntImm Low n)  -> fromIntegral n
      (IntImm High n) -> fromIntegral (shiftR n 8)
      (LabelImm _ _)  -> 0x00


{- |
Packs the code segment of a section. This function returns a binary structure which includes:

- A 2-byte little-endian field which is the number of code bytes packed
- The packed code bytes, packed iteratively with packCodeElem
-}
packCode :: Code -> [Word8]
packCode code = packDoubleWord (fromIntegral (length packedCode)) ++ packedCode
  where packedCode = concatMap packCodeElem code


{- |
Packs a single code element into bytes. InstCode elements are packed with packInst. LiteralCode
elements return their internal byte arrays directly.
-}
packCodeElem :: CodeElem -> [Word8]
packCodeElem (InstCode inst)     = packInst inst
packCodeElem (LiteralCode bytes) = bytes


packIndexSyms :: IndexSymTable -> [Word8]
packIndexSyms [] = packDoubleWord 0
packIndexSyms indexSyms
  =  packDoubleWord (fromIntegral (length indexSyms))
  ++ packedIndexSyms
  where packedIndexSyms = concatMap packIndexSym indexSyms


packIndexSym :: IndexSym -> [Word8]
packIndexSym (IndexSym index address)
  =  packDoubleWord address
  ++ packDoubleWord index


packIndexRelocs :: IndexRelocTable -> [Word8]
packIndexRelocs [] = packDoubleWord 0
packIndexRelocs indexRelocs
  = packDoubleWord (fromIntegral (length indexRelocs))
  ++ packedIndexRelocs
  where packedIndexRelocs = concatMap packIndexReloc indexRelocs


packIndexReloc :: IndexReloc -> [Word8]
packIndexReloc (IndexReloc kind address index)
  =  [fromIntegral (fromEnum kind)]
  ++ packDoubleWord address
  ++ packDoubleWord index


packIndexSec :: IndexSec -> [Word8]
packIndexSec (IndexSec name code indexSyms indexRelocs)
  =  packString name
  ++ packCode code
  ++ packIndexSyms indexSyms
  ++ packIndexRelocs indexRelocs
  

packBinaryObject :: BinaryObject -> [Word8]
packBinaryObject secs
  =  B.unpack (E.encodeUtf8 (T.pack magicHeader))
  ++ packDoubleWord (fromIntegral (length indexSecs))
  ++ packedIndexSecs
  ++ packDoubleWord (fromIntegral (length strings))
  ++ packedStrings
  where strings         = createStringTable secs
        indexSecs       = map (\sec -> applyIndexes sec strings) secs
        packedIndexSecs = concatMap packIndexSec indexSecs
        packedStrings   = concatMap packString strings


packNamedBinaryObject :: NamedBinaryObject -> [Word8]
packNamedBinaryObject (NamedBinaryObject name obj)
  =  packString name
  ++ packBinaryObject obj


unpackCode :: Parser Code
unpackCode = do
  len   <- unpackDoubleWord
  bytes <- count (fromIntegral len) word8
  return [LiteralCode bytes]


unpackIndexSyms :: Parser IndexSymTable
unpackIndexSyms = do
  num <- unpackDoubleWord
  count (fromIntegral num) unpackIndexSym


unpackIndexSym :: Parser IndexSym
unpackIndexSym = do
  address <- unpackDoubleWord
  index   <- unpackDoubleWord
  return (IndexSym index address)


unpackIndexRelocs :: Parser IndexRelocTable
unpackIndexRelocs = do
  num <- unpackDoubleWord
  count (fromIntegral num) unpackIndexReloc


unpackIndexReloc :: Parser IndexReloc
unpackIndexReloc = do
  kind    <- word8
  address <- unpackDoubleWord
  index   <- unpackDoubleWord
  return (IndexReloc (toEnum (fromIntegral kind)) address index)


unpackIndexSec :: Parser IndexSec
unpackIndexSec = do
  name        <- unpackString
  code        <- unpackCode
  indexSyms   <- unpackIndexSyms
  indexRelocs <- unpackIndexRelocs
  return (IndexSec name code indexSyms indexRelocs)


unpackBinaryObject :: Parser BinaryObject
unpackBinaryObject = do
  _            <- string (E.encodeUtf8 (T.pack magicHeader))
  numIndexSecs <- unpackDoubleWord
  indexSecs    <- count (fromIntegral numIndexSecs) unpackIndexSec
  numStrings   <- unpackDoubleWord
  strings      <- count (fromIntegral numStrings) unpackString
  return (map (\indexSec -> resolveIndexes indexSec strings) indexSecs)


unpackNamedBinaryObject :: Parser NamedBinaryObject
unpackNamedBinaryObject = do
  name <- unpackString
  obj  <- unpackBinaryObject
  return (NamedBinaryObject name obj)


getSecSize :: Sec -> Word16
getSecSize (Sec _ code _ _) = fromIntegral (getCodeSize code)
  where getCodeSize []                          = 0
        getCodeSize (InstCode _ : elems)        = 2 + getCodeSize elems
        getCodeSize (LiteralCode array : elems) = length array + getCodeSize elems


createStringTable :: [Sec] -> StringTable
createStringTable secs = nub (concatMap getStrings secs)
  where getStrings (Sec _ _ syms relocs) = union (map getFromSym syms) (map getFromReloc relocs)
        getFromSym (Sym label _)         = label
        getFromReloc (Reloc _ _ label)   = label


applyIndexes :: Sec -> StringTable -> IndexSec
applyIndexes (Sec name code syms relocs) strings
  = IndexSec name code (map applyToSym syms) (map applyToReloc relocs)
  where applyToSym (Sym label address) = case elemIndex label strings of
          Just index -> IndexSym (fromIntegral index) address
          Nothing    -> error "logic error: applyToSym called with malformed StringTable"
        applyToReloc (Reloc kind address label) = case elemIndex label strings of
          Just index -> IndexReloc kind address (fromIntegral index)
          Nothing    -> error "logic error: applyToReloc called with malformed StringTable"


resolveIndexes :: IndexSec -> StringTable -> Sec
resolveIndexes (IndexSec name code indexSyms indexRelocs) strings
  = Sec name code (map resolveInSym indexSyms) (map resolveInReloc indexRelocs)
  where resolveInSym (IndexSym index address)
          = Sym (strings !! (fromIntegral index)) address
        resolveInReloc (IndexReloc kind address index)
          = Reloc kind address (strings !! (fromIntegral index))

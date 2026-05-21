{- |
A basic object file containing linker sections with code, symbols, and relocations.

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


type BinaryObject = [Sec]


data NamedBinaryObject = NamedBinaryObject Label BinaryObject


data Sec = Sec Label Code SymTable RelocTable
  deriving (Show, Eq)


data IndexSec = IndexSec Label Code IndexSymTable IndexRelocTable


type Code = [CodeElem]


data CodeElem
  = InstCode Inst
  | LiteralCode [Word8]
  deriving (Show, Eq)


type SymTable = [Sym]


type IndexSymTable = [IndexSym]


data Sym = Sym Label Word16
  deriving (Show, Eq)


data IndexSym = IndexSym Word16 Word16
  deriving (Show, Eq)


type RelocTable = [Reloc]


type IndexRelocTable = [IndexReloc]


data Reloc = Reloc RelocType Word16 Label
  deriving (Show, Eq)


data IndexReloc = IndexReloc RelocType Word16 Word16
  deriving (Show, Eq)


data RelocType
  = LowReloc
  | HighReloc
  deriving (Show, Enum, Eq)


type StringTable = [Label]


magicHeader :: String
magicHeader = "MISA_LF "


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


packCode :: Code -> [Word8]
packCode code = packDoubleWord (fromIntegral (length packedCode)) ++ packedCode
  where packedCode = concatMap packCodeElem code


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

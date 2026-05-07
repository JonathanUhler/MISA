module Encoder (encodeProgram) where


import Grammar
import ObjectFile

import Data.Char (ord)
import qualified Data.Map as Map
import Data.Word (Word8, Word16)


splitSecs :: Program -> [(Label, Program)]
splitSecs stats = Map.toList (Map.map reverse (splitAcc "text" stats Map.empty))
  where
    splitAcc :: Label -> Program -> Map.Map String Program -> Map.Map String Program
    splitAcc _ [] acc = acc
    splitAcc _ (DirStat (SectionDir newSec) : rest) acc =
      let newAcc = Map.insertWith (++) newSec [] acc
      in splitAcc newSec rest newAcc
    splitAcc currSec (stat : rest) acc =
      let newAcc = Map.insertWith (++) currSec [stat] acc
      in splitAcc currSec rest newAcc


resolvePseudoInsts :: Program -> Program
resolvePseudoInsts [] = []
resolvePseudoInsts (stat : stats) =
  case stat of
    (InstStat inst) -> (map InstStat (resolvePseudoInst inst)) ++ resolvePseudoInsts stats
    _                    -> stat : resolvePseudoInsts stats


resolvePseudoInst :: Inst -> [Inst]
resolvePseudoInst inst = case inst of
  Add2Inst rd1 rd2 rs1 rs2 rs3 rs4
    -> [AddInst rd2 rs2 rs4, AdcInst rd1 rs1 rs3]
  And2Inst rd1 rd2 rs1 rs2 rs3 rs4
    -> [AndInst rd2 rs2 rs4, AndInst rd1 rs1 rs3]
  CallInst imm
    -> [SetInst RSCRATCH0 (highImm imm),
        SetInst RSCRATCH1 (lowImm imm),
        JalInst ALWAYS RSCRATCH0 RSCRATCH1]
  ClrInst
    -> [WsrInst FLAGS R0 R0]
  CmpInst rs1 rs2
    -> [SubInst R0 rs1 rs2]
  GotoInst imm
    -> [SetInst RSCRATCH0 (highImm imm),
        SetInst RSCRATCH1 (lowImm imm),
        JmpInst ALWAYS RSCRATCH0 RSCRATCH1]
  JaliInst cmp imm
    -> [SetInst RSCRATCH0 (highImm imm),
        SetInst RSCRATCH1 (lowImm imm),
        JalInst cmp RSCRATCH0 RSCRATCH1]
  JmpiInst cmp imm
    -> [SetInst RSCRATCH0 (highImm imm),
        SetInst RSCRATCH1 (lowImm imm),
        JmpInst cmp RSCRATCH0 RSCRATCH1]
  MovInst rd rs1
    -> [OrInst rd rs1 R0]
  Mov2Inst rd1 rd2 rs1 rs2
    -> [OrInst rd2 rs2 R0, OrInst rd1 rs1 R0]
  NopInst
    -> [OrInst R0 R0 R0]
  Or2Inst rd1 rd2 rs1 rs2 rs3 rs4
    -> [OrInst rd2 rs2 rs4, OrInst rd1 rs1 rs3]
  PopInst rd
    -> [RsrInst SADDR RSCRATCH0 RSCRATCH1,
        SetInst rd (IntImm Low 0x01),
        AddInst RSCRATCH1 RSCRATCH1 rd,
        AdcInst RSCRATCH0 RSCRATCH0 R0,
        LdInst rd RSCRATCH0 RSCRATCH1,
        WsrInst SADDR RSCRATCH0 RSCRATCH1]
  Pop2Inst rd1 rd2
    -> [RsrInst SADDR RSCRATCH0 RSCRATCH1,
        SetInst rd2 (IntImm Low 0x01),
        AddInst RSCRATCH1 RSCRATCH1 rd2,
        AdcInst RSCRATCH0 RSCRATCH0 R0,
        LdInst rd1 RSCRATCH0 RSCRATCH1,
        AddInst RSCRATCH1 RSCRATCH1 rd2,
        AdcInst RSCRATCH0 RSCRATCH0 R0,
        LdInst rd2 RSCRATCH0 RSCRATCH1,
        WsrInst SADDR RSCRATCH0 RSCRATCH1]
  PushInst rs
    -> [RsrInst SADDR RSCRATCH0 RSCRATCH1,
        StInst rs RSCRATCH0 RSCRATCH1,
        AddInst R0 R0 R0,  -- To set FLAGS.C = 0
        SbbInst RSCRATCH1 RSCRATCH1 R0,
        SbbInst RSCRATCH0 RSCRATCH0 R0,
        WsrInst SADDR RSCRATCH0 RSCRATCH1]
  Push2Inst rs1 rs2
    -> [RsrInst SADDR RSCRATCH0 RSCRATCH1,
        StInst rs2 RSCRATCH0 RSCRATCH1,
        AddInst R0 R0 R0,  -- To set FLAGS.C = 0
        SbbInst RSCRATCH1 RSCRATCH1 R0,
        SbbInst RSCRATCH0 RSCRATCH0 R0,
        StInst rs1 RSCRATCH0 RSCRATCH1,
        AddInst R0 R0 R0,  -- To set FLAGS.C = 0
        SbbInst RSCRATCH1 RSCRATCH1 R0,
        SbbInst RSCRATCH0 RSCRATCH0 R0,
        WsrInst SADDR RSCRATCH0 RSCRATCH1]
  RetInst
    -> [RsrInst RADDR RSCRATCH0 RSCRATCH1, JmpInst ALWAYS RSCRATCH0 RSCRATCH1]
  Rrc2Inst rd1 rd2 rs1 rs2
    -> [RrcInst rd1 rs1, RrcInst rd2 rs2]
  Set2Inst rs1 rs2 imm
    -> [SetInst rs1 (highImm imm), SetInst rs2 (lowImm imm)]
  Sub2Inst rd1 rd2 rs1 rs2 rs3 rs4
    -> [SubInst rd2 rs2 rs4, SbbInst rd1 rs1 rs3]
  Xor2Inst rd1 rd2 rs1 rs2 rs3 rs4
    -> [XorInst rd2 rs2 rs4, XorInst rd1 rs1 rs3]
  _ -> [inst]
  where
    lowImm  (IntImm   _ n) = IntImm   Low  n
    lowImm  (LabelImm _ n) = LabelImm Low  n
    highImm (IntImm   _ n) = IntImm   High n
    highImm (LabelImm _ n) = LabelImm High n


extractCode :: Program -> Code
extractCode []                                       = []
extractCode (InstStat inst             : stats) = InstCode inst      : extractCode stats
extractCode (DirStat (WordDir word)    : stats) = LiteralCode [word] : extractCode stats
extractCode (DirStat (ArrayDir array)  : stats) = LiteralCode array  : extractCode stats
extractCode (DirStat (AsciiDir value)  : stats) = LiteralCode array  : extractCode stats
  where array = map (fromIntegral . ord) value
extractCode (DirStat (AsciizDir value) : stats) = LiteralCode array  : extractCode stats
  where array = (map (fromIntegral . ord) value) ++ [0x00]
extractCode (DirStat (SpaceDir size)   : stats) = LiteralCode array : extractCode stats
  where array = replicate (fromIntegral size) (0x00 :: Word8)
extractCode (_                         : stats) = extractCode stats


extractSyms :: Program -> SymTable
extractSyms stats = getWithPc stats 0
  where
    getWithPc :: Program -> Word16 -> SymTable
    getWithPc [] _                              = []
    getWithPc (InstStat _            : rest) pc = getWithPc rest (pc + 2)
    getWithPc (DirStat (WordDir _)   : rest) pc = getWithPc rest (pc + 1)
    getWithPc (DirStat (ArrayDir a)  : rest) pc = getWithPc rest (pc + fromIntegral (length a))
    getWithPc (DirStat (AsciiDir v)  : rest) pc = getWithPc rest (pc + fromIntegral (length v))
    getWithPc (DirStat (AsciizDir v) : rest) pc = getWithPc rest (pc + fromIntegral (length v + 1))
    getWithPc (DirStat (SpaceDir s)  : rest) pc = getWithPc rest (pc + s)
    getWithPc (LabelStat label       : rest) pc = [Sym label pc] ++ getWithPc rest pc
    getWithPc (_                     : rest) pc = getWithPc rest (pc + 0)


extractRelocs :: Program -> RelocTable
extractRelocs statements = getWithPc statements 0
  where
    getWithPc :: Program -> Word16 -> RelocTable
    getWithPc [] _                              = []
    getWithPc (InstStat inst         : rest) pc = case extractLabelImm inst of
      Just (LabelImm part sec) -> [Reloc (fromPart part) (pc + 1) sec] ++ getWithPc rest (pc + 2)
      _                        -> getWithPc rest (pc + 2)
    getWithPc (DirStat (WordDir _)   : rest) pc = getWithPc rest (pc + 1)
    getWithPc (DirStat (ArrayDir a)  : rest) pc = getWithPc rest (pc + fromIntegral (length a))
    getWithPc (DirStat (AsciiDir v)  : rest) pc = getWithPc rest (pc + fromIntegral (length v))
    getWithPc (DirStat (AsciizDir v) : rest) pc = getWithPc rest (pc + fromIntegral (length v + 1))
    getWithPc (DirStat (SpaceDir s)  : rest) pc = getWithPc rest (pc + s)
    getWithPc (_                     : rest) pc = getWithPc rest pc

    extractLabelImm :: Inst -> Maybe Imm
    extractLabelImm (SetInst  _   label) = Just label
    extractLabelImm (Set2Inst _ _ label) = Just label
    extractLabelImm _                    = Nothing

    fromPart :: ImmPart -> RelocType
    fromPart Full = LowReloc
    fromPart Low  = LowReloc
    fromPart High = HighReloc


encoderCreateSec :: (Label, Program) -> Sec
encoderCreateSec (name, statements) = Sec name code syms relocs
  where code   = extractCode statements
        syms   = extractSyms statements
        relocs = extractRelocs statements


encodeProgram :: Program -> BinaryObject
encodeProgram stats = map encoderCreateSec (splitSecs (resolvePseudoInsts stats))

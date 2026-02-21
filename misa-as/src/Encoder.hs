module Encoder (encodeProgram) where


import Grammar
import ObjectFile

import qualified Data.Map as Map
import Data.Word (Word16)


splitSecs :: Program -> [(Label, Program)]
splitSecs stats = Map.toList (Map.map reverse (splitAcc "text" stats Map.empty))
  where
    splitAcc :: Label -> Program -> Map.Map String Program -> Map.Map String Program
    splitAcc _ [] acc = acc
    splitAcc _ (DirStatement (SectionDir newSec) : rest) acc =
      let newAcc = Map.insertWith (++) newSec [] acc
      in splitAcc newSec rest newAcc
    splitAcc currSec (stat : rest) acc =
      let newAcc = Map.insertWith (++) currSec [stat] acc
      in splitAcc currSec rest newAcc


resolvePseudoInsts :: Program -> Program
resolvePseudoInsts [] = []
resolvePseudoInsts (stat : stats) =
  case stat of
    (InstStatement inst) -> (map InstStatement (resolvePseudoInst inst)) ++ resolvePseudoInsts stats
    _                    -> stat : resolvePseudoInsts stats


resolvePseudoInst :: Inst -> [Inst]
resolvePseudoInst inst = case inst of
  NopInst              -> [OrInst R0 R0 R0]
  NotInst  rd  rs1     -> [XorInst rd rs1 rs1]
  MovInst  rd  rs1     -> [OrInst rd rs1 R0]
  CmpInst  rs1 rs2     -> [SubInst R0 rs1 rs2]
  SetdInst rs1 rs2 imm -> [SetInst rs1 (highImm imm), SetInst rs2 (lowImm imm)]
  CallInst rs1 rs2     -> [JalInst ALWAYS rs1 rs2]
  RetInst              -> [RsrInst RADDR RY RZ, JmpInst ALWAYS RY RZ]
  ClrInst              -> [WsrInst FLAGS R0 R0]
  _                    -> [inst]
  where
    lowImm  (IntImm   _ n) = IntImm   Low  n
    lowImm  (LabelImm _ n) = LabelImm Low  n
    highImm (IntImm   _ n) = IntImm   High n
    highImm (LabelImm _ n) = LabelImm High n


extractCode :: Program -> Code
extractCode []                                      = []
extractCode (InstStatement inst            : stats) = InstCode inst      : extractCode stats
extractCode (DirStatement (WordDir word)   : stats) = LiteralCode [word] : extractCode stats
extractCode (DirStatement (ArrayDir array) : stats) = LiteralCode array  : extractCode stats
extractCode (_ : stats)                             = extractCode stats


extractSyms :: Program -> SymTable
extractSyms stats = getWithPc stats 0
  where
    getWithPc :: Program -> Word16 -> SymTable
    getWithPc [] _                                  = []
    getWithPc (InstStatement _           : rest) pc = getWithPc rest (pc + 2)
    getWithPc (DirStatement (WordDir _)  : rest) pc = getWithPc rest (pc + 1)
    getWithPc (DirStatement (ArrayDir a) : rest) pc = getWithPc rest (pc + fromIntegral (length a))
    getWithPc (LabelStatement label      : rest) pc = [Sym label pc] ++ getWithPc rest pc
    getWithPc (_                         : rest) pc = getWithPc rest (pc + 0)


extractRelocs :: Program -> RelocTable
extractRelocs statements = getWithPc statements 0
  where
    getWithPc :: Program -> Word16 -> RelocTable
    getWithPc [] _                                  = []
    getWithPc (InstStatement inst        : rest) pc = case extractLabelImm inst of
      Just (LabelImm part sec) -> [Reloc (fromPart part) (pc + 1) sec] ++ getWithPc rest (pc + 2)
      _                        -> getWithPc rest (pc + 2)
    getWithPc (DirStatement (WordDir _)  : rest) pc = getWithPc rest (pc + 1)
    getWithPc (DirStatement (ArrayDir a) : rest) pc = getWithPc rest (pc + fromIntegral (length a))
    getWithPc (_                         : rest) pc = getWithPc rest pc

    extractLabelImm :: Inst -> Maybe Imm
    extractLabelImm (SetInst  _   label) = Just label
    extractLabelImm (SetdInst _ _ label) = Just label
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

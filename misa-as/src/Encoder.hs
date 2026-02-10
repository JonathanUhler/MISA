module Encoder (encoderRun) where


import Grammar
import ObjectFile

import qualified Data.Map as Map
import Data.Word (Word16)


encoderSplitSections :: Program -> [(Label, Program)]
encoderSplitSections statements
  = Map.toList (Map.map reverse (encoderSplitAcc "text" statements Map.empty))
  where encoderSplitAcc _ [] acc = acc
        encoderSplitAcc _ (DirectiveStatement (SectionDirective newSection) : rest) acc =
          let newAcc = Map.insertWith (++) newSection [] acc
          in encoderSplitAcc newSection rest newAcc
        encoderSplitAcc currSection (statement : rest) acc =
          let newAcc = Map.insertWith (++) currSection [statement] acc
          in encoderSplitAcc currSection rest newAcc


encoderGetInstructions :: Program -> [Instruction]
encoderGetInstructions [] = []
encoderGetInstructions (InstructionStatement instruction : statements)
  = encoderGetInstructions statements ++ [instruction]
encoderGetInstructions (_ : statements) = encoderGetInstructions statements


encoderGetSymbols :: Program -> SymbolTable
encoderGetSymbols statements = encoderGetSymbolsWithPc statements 0
  where encoderGetSymbolsWithPc :: Program -> Word16 -> SymbolTable
        encoderGetSymbolsWithPc [] _ = []
        encoderGetSymbolsWithPc (InstructionStatement _ : rest) address
          = encoderGetSymbolsWithPc rest (address + 2)
        encoderGetSymbolsWithPc (LabelStatement label : rest) address
          = [Symbol label address] ++ encoderGetSymbolsWithPc rest address
        encoderGetSymbolsWithPc (_ : rest) address
          = encoderGetSymbolsWithPc rest address


encoderGetRelocations :: Program -> RelocationTable
encoderGetRelocations _ = []  -- TODO: Implement when instructions with label immediates are added


encoderCreateSection :: (Label, Program) -> Section
encoderCreateSection (name, statements) = Section name instructions symbols relocations
  where instructions = encoderGetInstructions statements
        symbols      = encoderGetSymbols statements
        relocations  = encoderGetRelocations statements


encoderRun :: Program -> BinaryObject
encoderRun [] = []
encoderRun statements = map encoderCreateSection (encoderSplitSections statements)

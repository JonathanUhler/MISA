{- |
A simple encoder that converts a program (a list of statements) into a basic object file.

Author: Jonathan Uhler
-}
module Encoder (encoderRun) where


import Grammar
import ObjectFile

import qualified Data.Map as Map
import Data.Word (Word16)


{- |
Splits the provided program into a list of labeled sections by searching for .section directives
(`DirectiveStatement (SectionDirective)` instances in the input program).

If the input program does not start with (or does not contain any) `SectionDirective`s, a
default section called "text" will be inserted at the start of the program.

Multiple section directives with the same name can appear in the input program, and their
statements will be merged into one tuple in the output list in chronological order.

This function is intended to be called once with the full program. Subsequent calls will not
be helpful, as all the statements will be grouped into the default "text" section, which may
not be faithful to the original program.

This function returns a list of pairs of section names and sub-programs which are all the
statements in sections with that name. No parsing of the statements (e.g. handling other
directives, tracking labels, etc) is performed by this function.
-}
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


{- |
Collects all of the instruction statements and memory directives (e.g. .word, .array) from the
input program and returns them as a list of code elements, which are the `Instruction`s themselves
or lists of bytes that should literally be inserted into the binary file.

This function should be called with a sub-program representing a single section after
calling `encoderSplitSection`. It is assumed that no section directives are present in the
program.
-}
encoderGetCode :: Program -> Code
encoderGetCode [] = []
encoderGetCode (InstructionStatement instruction : statements)
  = encoderGetCode statements ++ [InstructionCode instruction]
encoderGetCode (DirectiveStatement (WordDirective word) : statements)
  = encoderGetCode statements ++ [LiteralCode [word]]
encoderGetCode (DirectiveStatement (ArrayDirective array) : statements)
  = encoderGetCode statements ++ [LiteralCode array]
encoderGetCode (_ : statements) = encoderGetCode statements


{- |
Collects all of the label definitions from the input program and returns them as a symbol table
with their addresses/PC values relative to the beginning of the input program. The address of
labels is only incremented for each instruction statement or memory directive (e.g. .array).

This function should be called with a sub-program representing a single section after
calling `encoderSplitSection`. It is assumed that no section directives are present in the
program.
-}
encoderGetSymbols :: Program -> SymbolTable
encoderGetSymbols statements = encoderGetSymbolsWithPc statements 0
  where encoderGetSymbolsWithPc :: Program -> Word16 -> SymbolTable
        encoderGetSymbolsWithPc [] _ = []
        encoderGetSymbolsWithPc (InstructionStatement _ : rest) address
          = encoderGetSymbolsWithPc rest (address + 2)
        encoderGetSymbolsWithPc (DirectiveStatement (WordDirective _) : rest) address
          = encoderGetSymbolsWithPc rest (address + 1)
        encoderGetSymbolsWithPc (DirectiveStatement (ArrayDirective array) : rest) address
          = encoderGetSymbolsWithPc rest (address + fromIntegral (length array))
        encoderGetSymbolsWithPc (LabelStatement label : rest) address
          = [Symbol label address] ++ encoderGetSymbolsWithPc rest address
        encoderGetSymbolsWithPc (_ : rest) address
          = encoderGetSymbolsWithPc rest address


{- |
Finds all of the references to label-type immediate values in the instructions in the input program
and returns them as a relocation table with their addresses/PC values relative to the beginning
of the input program.

This function should be called with a sub-program representing a single section after
calling `encoderSplitSection`. It is assumed that no section directives are present in the
program.
-}
encoderGetRelocations :: Program -> RelocationTable
encoderGetRelocations _ = []  -- TODO: Implement when instructions with label immediates are added


{- |
Creates a `Section` with code, a symbol table, and a relocation table from a section name and
list of program statements in that section.
-}
encoderCreateSection :: (Label, Program) -> Section
encoderCreateSection (name, statements) = Section name code symbols relocations
  where code        = encoderGetCode statements
        symbols     = encoderGetSymbols statements
        relocations = encoderGetRelocations statements


{- |
Encodes a program into a basic object file.

The object file is defined as a list of sections to be placed in memory in the final binary by
the linker. For more information on the object file format, see `BinaryObject`.
-}
encoderRun :: Program -> BinaryObject
encoderRun [] = []
encoderRun statements = map encoderCreateSection (encoderSplitSections statements)

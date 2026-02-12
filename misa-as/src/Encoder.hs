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


encoderResolvePseudoInstructions :: Program -> Program
encoderResolvePseudoInstructions [] = []
encoderResolvePseudoInstructions (statement : statements) =
  case statement of
    (InstructionStatement instruction) ->
      (map InstructionStatement resolved) ++ encoderResolvePseudoInstructions statements
      where resolved = case instruction of
              NotInstruction rd rs1      -> [XorInstruction rd rs1 rs1]
              MovInstruction rd rs1      -> [OrInstruction rd rs1 R0]
              LdiInstruction rs1 rs2 imm -> [LiInstruction rs1 (encoderGetLowImmediate imm),
                                             LiInstruction rs2 (encoderGetHighImmediate imm)]
              CallInstruction imm        -> [LiInstruction R1 (encoderGetLowImmediate imm),
                                             LiInstruction R2 (encoderGetHighImmediate imm),
                                             SaInstruction R2 R1,
                                             JlzInstruction R0 (IntImmediate Low 0)]
              EntryInstruction           -> [LaInstruction R2 R1]
              RetInstruction             -> [SaInstruction R2 R1,
                                             JzInstruction R0 (IntImmediate Low 0)]
              PushInstruction rs1        -> [SaInstruction R4 R3,
                                             SwInstruction rs1 (IntImmediate Low 0),
                                             LiInstruction R15 (IntImmediate Low 0xFF),
                                             AddInstruction R3 R3 R15,
                                             AdcInstruction R4 R4 R15]
              PopInstruction rs1         -> [LiInstruction R15 (IntImmediate Low 1),
                                             AddInstruction R3 R3 R15,
                                             AdcInstruction R4 R4 R0,
                                             SaInstruction R4 R3,
                                             LwInstruction rs1 (IntImmediate Low 0)]
              _                          -> [instruction]
    _ -> statement : encoderResolvePseudoInstructions statements


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
  = InstructionCode instruction : encoderGetCode statements
encoderGetCode (DirectiveStatement (WordDirective word) : statements)
  = LiteralCode [word] : encoderGetCode statements
encoderGetCode (DirectiveStatement (ArrayDirective array) : statements)
  = LiteralCode array : encoderGetCode statements
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
encoderGetSymbols statements = getSymbolsWithPc statements 0
  where getSymbolsWithPc :: Program -> Word16 -> SymbolTable
        getSymbolsWithPc [] _ = []
        getSymbolsWithPc (InstructionStatement _ : rest) address
          = getSymbolsWithPc rest (address + 2)
        getSymbolsWithPc (DirectiveStatement (WordDirective _) : rest) address
          = getSymbolsWithPc rest (address + 1)
        getSymbolsWithPc (DirectiveStatement (ArrayDirective array) : rest) address
          = getSymbolsWithPc rest (address + fromIntegral (length array))
        getSymbolsWithPc (LabelStatement label : rest) address
          = [Symbol label address] ++ getSymbolsWithPc rest address
        getSymbolsWithPc (_ : rest) address
          = getSymbolsWithPc rest address


{- |
Finds all of the references to label-type immediate values in the instructions in the input program
and returns them as a relocation table with their addresses/PC values relative to the beginning
of the input program.

This function should be called with a sub-program representing a single section after
calling `encoderSplitSection`. It is assumed that no section directives are present in the
program.
-}
encoderGetRelocations :: Program -> RelocationTable
encoderGetRelocations statements = getRelocationsWithPc statements 0
  where getRelocationsWithPc :: Program -> Word16 -> RelocationTable
        getRelocationsWithPc [] _ = []
        getRelocationsWithPc (InstructionStatement instruction : rest) address
          = case encoderGetLabelImmediate instruction of
              Just (LabelImmediate part label) -> [Relocation (fromPart part) (address + 1) label]
                                               ++ getRelocationsWithPc rest (address + 2)
              _                                -> getRelocationsWithPc rest (address + 2)
        getRelocationsWithPc (DirectiveStatement (WordDirective _) : rest) address
          = getRelocationsWithPc rest (address + 1)
        getRelocationsWithPc (DirectiveStatement (ArrayDirective array) : rest) address
          = getRelocationsWithPc rest (address + fromIntegral (length array))
        getRelocationsWithPc (_ : rest) address
          = getRelocationsWithPc rest address

        fromPart :: ImmediatePart -> RelocationType
        fromPart Full = LowRelocation
        fromPart Low  = LowRelocation
        fromPart High = HighRelocation


encoderGetLabelImmediate :: Instruction -> Maybe Immediate
encoderGetLabelImmediate (LwInstruction _ label)  = Just label
encoderGetLabelImmediate (SwInstruction _ label)  = Just label
encoderGetLabelImmediate (LiInstruction _ label)  = Just label
encoderGetLabelImmediate (JlzInstruction _ label) = Just label
encoderGetLabelImmediate (JzInstruction _ label)  = Just label
encoderGetLabelImmediate (HaltInstruction label)  = Just label
encoderGetLabelImmediate _                        = Nothing


encoderGetLowImmediate :: Immediate -> Immediate
encoderGetLowImmediate (IntImmediate _ n)       = IntImmediate Low n
encoderGetLowImmediate (LabelImmediate _ label) = LabelImmediate Low label


encoderGetHighImmediate :: Immediate -> Immediate
encoderGetHighImmediate (IntImmediate _ n)       = IntImmediate High n
encoderGetHighImmediate (LabelImmediate _ label) = LabelImmediate High label


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
encoderRun statements =
  map encoderCreateSection (encoderSplitSections (encoderResolvePseudoInstructions statements))

{- |
A simple parser that processes a list of tokens into a program containing a list of statements.

Author: Jonathan Uhler
-}
module Parser (parserRun) where


import Grammar
import Lexer

import Control.Applicative ((<|>))
import Data.Word (Word8)


{- |
Returns whether the provided integer literal could fit in an unsigned integer type of the
provided width.
-}
isValidImm :: Int -> Int -> Bool
isValidImm imm width = 0 <= imm && imm <= 2 ^ width - 1


{- |
Tries to parse an assembly instruction from the head of the provided list of tokens.

If parsing succeeds, a tuple containing the instruction and the remaining tokens with the consumed
tokens removed is returned. Otherwise `Nothing` is returned.
-}
parserGetInst :: [Token] -> Maybe (Statement, [Token])
-- Instructions with three registers
parserGetInst (OpcodeToken ADD : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (AddInstruction rd rs1 rs2), tokens)
parserGetInst (OpcodeToken ADC : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (AdcInstruction rd rs1 rs2), tokens)
parserGetInst (OpcodeToken SUB : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (SubInstruction rd rs1 rs2), tokens)
parserGetInst (OpcodeToken AND : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (AndInstruction rd rs1 rs2), tokens)
parserGetInst (OpcodeToken OR : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (OrInstruction rd rs1 rs2), tokens)
parserGetInst (OpcodeToken XOR : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (XorInstruction rd rs1 rs2), tokens)
-- Instructions with a register and an immediate
parserGetInst (OpcodeToken LW : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (LwInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken SW : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (SwInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken LI : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (LiInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken JLZ : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (JlzInstruction rd (fromIntegral imm)), tokens)
-- Instructions with two registers
parserGetInst (OpcodeToken LA : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (LaInstruction rs1 rs2), tokens)
parserGetInst (OpcodeToken SA : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (SaInstruction rs1 rs2), tokens)
-- Instructions with just an immediate
parserGetInst (OpcodeToken HALT : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (HaltInstruction (fromIntegral imm)), tokens)
-- Not a known instruction
parserGetInst _ = Nothing


{- |
Tries to parse a label definition from the head of the provided list of tokens.

If parsing succeeds, a tuple containing the label and the remaining tokens with the consumed
tokens removed is returned. Otherwise `Nothing` is returned.
-}
parserGetLabel :: [Token] -> Maybe (Statement, [Token])
parserGetLabel (IdentifierToken label : ColonToken : tokens) = Just (LabelStatement label, tokens)
parserGetLabel _                                             = Nothing


{- |
Builds an array of words for a .array directive by consuming as many 8-bit `NumberToken`s from
the provided list of tokens as possible.

Note that this function can return an array of zero words. If the caller does not support
zero-length arrays, it is their responsibility to match and filter for [].

Returns a tuple containing the .array directive contents/words and the remaining tokens with all
consumed `NumberToken`s removed.
-}
buildArrayDirective :: [Token] -> ([Word8], [Token])
buildArrayDirective (NumberToken x : tokens) | isValidImm x 8
  = (fromIntegral x : xs, rest)
  where (xs, rest) = buildArrayDirective tokens
buildArrayDirective tokens = ([], tokens)


{- |
Tries to parse a directive from the head of the provided list of tokens.

If parsing succeeds, a tuple containing the directive and its payload (if applicable) as well as
the remaining tokens with the consumed tokens removed is returned. Otherwise `Nothing` is returned.
-}
parserGetDirective :: [Token] -> Maybe (Statement, [Token])
parserGetDirective (PeriodToken : IdentifierToken "word" : NumberToken x : tokens) | isValidImm x 8
  = Just (DirectiveStatement (WordDirective (fromIntegral x)), tokens)
parserGetDirective (PeriodToken : IdentifierToken "array" : tokens) =
  if length array > 0 then
    Just (DirectiveStatement (ArrayDirective array), rest)
  else
    Nothing
  where (array, rest) = buildArrayDirective tokens
parserGetDirective (PeriodToken : IdentifierToken "section" : IdentifierToken section : tokens)
  = Just (DirectiveStatement (SectionDirective section), tokens)
parserGetDirective _ = Nothing


{- |
Tries to parse any type of statement from the head of the provided list of tokens.

The first type of statement that matches the list of tokens will be returned, along with the
remainder of the token list as a tuple. If no statement matches the tokens, `Nothing` is returned.
-}
parserGetNextStatement :: [Token] -> Maybe (Statement, [Token])
parserGetNextStatement tokens
  =   parserGetInst tokens
  <|> parserGetLabel tokens
  <|> parserGetDirective tokens


{- |
Runs semantic analysis on the provided list of tokens representing an assembly program.

The parser returns a tuple containing the program (list of statements) parsed from the tokens
in the order they appear, and a list of any unparsed tokens.

When a semantic error occurs, the parse will immediately stop and return the partially parsed
input and remaining tokens. The semantic error occured at the first token in the unparsed list.

The caller should check that the returned unparsed list of tokens is empty, which indicates that
the entire input was parsed successfully. If the list of returned tokens is non-empty, a semantic
error occured.
-}
parserRun :: [Token] -> (Program, [Token])
parserRun [] = ([], [])
parserRun tokens =
  case parserGetNextStatement tokens of
    Just (statement, rest) -> (statement : statements, unparsed)
      where (statements, unparsed) = parserRun rest
    Nothing                -> ([], tokens)

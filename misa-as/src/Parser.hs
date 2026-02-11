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
Tries to parse an assembly instruction from the head of the provided list of tokens.

If parsing succeeds, a tuple containing the instruction and the remaining tokens with the consumed
tokens removed is returned. Otherwise `Nothing` is returned.
-}
parserGetInst :: [Token] -> Maybe (Statement, [Token])
parserGetInst tokens = case tokens of
  (OpcodeToken op : RegisterToken rd : RegisterToken rs1 : RegisterToken rs2 : rest) ->
    case op of
      ADD -> Just (InstructionStatement (AddInstruction rd rs1 rs2), rest)
      ADC -> Just (InstructionStatement (AdcInstruction rd rs1 rs2), rest)
      SUB -> Just (InstructionStatement (SubInstruction rd rs1 rs2), rest)
      AND -> Just (InstructionStatement (AndInstruction rd rs1 rs2), rest)
      OR  -> Just (InstructionStatement (OrInstruction rd rs1 rs2), rest)
      XOR -> Just (InstructionStatement (XorInstruction rd rs1 rs2), rest)
      _   -> Nothing
  (OpcodeToken op : RegisterToken rd : NumberToken imm : rest) ->
    case op of
      LW  -> Just (InstructionStatement (LwInstruction rd (IntImmediate imm)), rest)
      SW  -> Just (InstructionStatement (SwInstruction rd (IntImmediate imm)), rest)
      LI  -> Just (InstructionStatement (LiInstruction rd (IntImmediate imm)), rest)
      JLZ -> Just (InstructionStatement (JlzInstruction rd (IntImmediate imm)), rest)
      JZ  -> Just (InstructionStatement (JzInstruction rd (IntImmediate imm)), rest)
      _   -> Nothing
  (OpcodeToken op : RegisterToken rd : IdentifierToken imm : rest) ->
    case op of
      LW  -> Just (InstructionStatement (LwInstruction rd (LabelImmediate imm)), rest)
      SW  -> Just (InstructionStatement (SwInstruction rd (LabelImmediate imm)), rest)
      LI  -> Just (InstructionStatement (LiInstruction rd (LabelImmediate imm)), rest)
      JLZ -> Just (InstructionStatement (JlzInstruction rd (LabelImmediate imm)), rest)
      JZ  -> Just (InstructionStatement (JzInstruction rd (LabelImmediate imm)), rest)
      _   -> Nothing
  (OpcodeToken op : RegisterToken rs1 : RegisterToken rs2 : rest) ->
    case op of
      LA -> Just (InstructionStatement (LaInstruction rs1 rs2), rest)
      SA -> Just (InstructionStatement (SaInstruction rs1 rs2), rest)
      _  -> Nothing
  (OpcodeToken op : NumberToken imm : rest) ->
    case op of
      HALT -> Just (InstructionStatement (HaltInstruction (IntImmediate imm)), rest)
      _    -> Nothing
  (OpcodeToken op : IdentifierToken imm : rest) ->
    case op of
      HALT -> Just (InstructionStatement (HaltInstruction (LabelImmediate imm)), rest)
      _    -> Nothing
  _ -> Nothing


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
buildArrayDirective (NumberToken x : tokens)
  = (fromIntegral x : xs, rest)
  where (xs, rest) = buildArrayDirective tokens
buildArrayDirective tokens = ([], tokens)


{- |
Tries to parse a directive from the head of the provided list of tokens.

If parsing succeeds, a tuple containing the directive and its payload (if applicable) as well as
the remaining tokens with the consumed tokens removed is returned. Otherwise `Nothing` is returned.
-}
parserGetDirective :: [Token] -> Maybe (Statement, [Token])
parserGetDirective (PeriodToken : IdentifierToken "word" : NumberToken x : tokens)
  = Just (DirectiveStatement (WordDirective (fromIntegral x)), tokens)
parserGetDirective (PeriodToken : IdentifierToken "array" : tokens)
  | array /= [] = Just (DirectiveStatement (ArrayDirective array), rest)
  | otherwise   = Nothing
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

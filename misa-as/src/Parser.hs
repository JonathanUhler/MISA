module Parser (parserRun) where


import Grammar
import Lexer

import Control.Applicative ((<|>))
import Data.Word (Word8)


isValidImm :: Int -> Int -> Bool
isValidImm imm width = 0 <= imm && imm <= 2 ^ width - 1


parserGetInst :: [Token] -> Maybe (Statement, [Token])
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

parserGetInst (OpcodeToken LW : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (LwInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken SW : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (SwInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken LI : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (LiInstruction rd (fromIntegral imm)), tokens)
parserGetInst (OpcodeToken JLZ : RegisterToken rd : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (JlzInstruction rd (fromIntegral imm)), tokens)

parserGetInst (OpcodeToken LA : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (LaInstruction rs1 rs2), tokens)
parserGetInst (OpcodeToken SA : RegisterToken rs1 : RegisterToken rs2 : tokens)
  = Just (InstructionStatement (SaInstruction rs1 rs2), tokens)

parserGetInst (OpcodeToken HALT : NumberToken imm : tokens) | isValidImm imm 8
  = Just (InstructionStatement (HaltInstruction (fromIntegral imm)), tokens)

parserGetInst _ = Nothing

  
parserGetLabel :: [Token] -> Maybe (Statement, [Token])
parserGetLabel (IdentifierToken label : ColonToken : tokens) = Just (LabelStatement label, tokens)
parserGetLabel _                                             = Nothing


buildArrayDirective :: [Token] -> ([Word8], [Token])
buildArrayDirective (NumberToken x : tokens) | isValidImm x 8
  = (fromIntegral x : xs, rest)
  where (xs, rest) = buildArrayDirective tokens
buildArrayDirective tokens = ([], tokens)


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


parserGetNextStatement :: [Token] -> Maybe (Statement, [Token])
parserGetNextStatement tokens
  =   parserGetInst tokens
  <|> parserGetLabel tokens
  <|> parserGetDirective tokens


parserRun :: [Token] -> (Program, [Token])
parserRun [] = ([], [])
parserRun tokens =
  case parserGetNextStatement tokens of
    Just (statement, rest) -> (statement : statements, unparsed)
      where (statements, unparsed) = parserRun rest
    Nothing                -> ([], tokens)

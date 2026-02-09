{-# LANGUAGE ViewPatterns #-}


module Lexer (lexerRun) where


import Grammar

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (stripPrefix)
import Numeric (readDec, readBin, readHex, readOct)


data Token
  = TokenColon
  | TokenPeriod
  | TokenDirective Directive
  | TokenOpcode Opcode
  | TokenRegister Register
  | TokenIdentifier String
  | TokenNumber Int
  | TokenUnknown String
  deriving Show


lexerRun :: String -> [Token]
lexerRun [] = []
-- Whitespace
lexerRun (c:cs) | isSpace c = lexerRun rest
  where (_, rest) = span isSpace cs
-- Comment
lexerRun (';':cs) = lexerRun rest
  where (_, rest) = span (\x -> x /= '\n') cs
-- Colon
lexerRun (':':cs) = TokenColon : lexerRun cs
-- Period
lexerRun ('.':cs) = TokenPeriod : lexerRun cs
-- Directive name
lexerRun (stripPrefix "word"    -> Just rest) = TokenDirective WordDirective    : lexerRun rest
lexerRun (stripPrefix "array"   -> Just rest) = TokenDirective ArrayDirective   : lexerRun rest
lexerRun (stripPrefix "section" -> Just rest) = TokenDirective SectionDirective : lexerRun rest
-- Opcode name
lexerRun (stripPrefix "ADD"  -> Just rest) = TokenOpcode ADD  : lexerRun rest
lexerRun (stripPrefix "ADC"  -> Just rest) = TokenOpcode ADC  : lexerRun rest
lexerRun (stripPrefix "SUB"  -> Just rest) = TokenOpcode SUB  : lexerRun rest
lexerRun (stripPrefix "AND"  -> Just rest) = TokenOpcode AND  : lexerRun rest
lexerRun (stripPrefix "OR"   -> Just rest) = TokenOpcode OR   : lexerRun rest
lexerRun (stripPrefix "XOR"  -> Just rest) = TokenOpcode XOR  : lexerRun rest
lexerRun (stripPrefix "LW"   -> Just rest) = TokenOpcode LW   : lexerRun rest
lexerRun (stripPrefix "SW"   -> Just rest) = TokenOpcode SW   : lexerRun rest
lexerRun (stripPrefix "LA"   -> Just rest) = TokenOpcode LA   : lexerRun rest
lexerRun (stripPrefix "SA"   -> Just rest) = TokenOpcode SA   : lexerRun rest
lexerRun (stripPrefix "LI"   -> Just rest) = TokenOpcode LI   : lexerRun rest
lexerRun (stripPrefix "JLZ"  -> Just rest) = TokenOpcode JLZ  : lexerRun rest
lexerRun (stripPrefix "HALT" -> Just rest) = TokenOpcode HALT : lexerRun rest
-- Register name
lexerRun (stripPrefix "R0"  -> Just rest) = TokenRegister R0  : lexerRun rest
lexerRun (stripPrefix "R1"  -> Just rest) = TokenRegister R1  : lexerRun rest
lexerRun (stripPrefix "R2"  -> Just rest) = TokenRegister R2  : lexerRun rest
lexerRun (stripPrefix "R3"  -> Just rest) = TokenRegister R3  : lexerRun rest
lexerRun (stripPrefix "R4"  -> Just rest) = TokenRegister R4  : lexerRun rest
lexerRun (stripPrefix "R5"  -> Just rest) = TokenRegister R5  : lexerRun rest
lexerRun (stripPrefix "R6"  -> Just rest) = TokenRegister R6  : lexerRun rest
lexerRun (stripPrefix "R7"  -> Just rest) = TokenRegister R7  : lexerRun rest
lexerRun (stripPrefix "R8"  -> Just rest) = TokenRegister R8  : lexerRun rest
lexerRun (stripPrefix "R9"  -> Just rest) = TokenRegister R9  : lexerRun rest
lexerRun (stripPrefix "R10" -> Just rest) = TokenRegister R10 : lexerRun rest
lexerRun (stripPrefix "R11" -> Just rest) = TokenRegister R11 : lexerRun rest
lexerRun (stripPrefix "R12" -> Just rest) = TokenRegister R12 : lexerRun rest
lexerRun (stripPrefix "R13" -> Just rest) = TokenRegister R13 : lexerRun rest
lexerRun (stripPrefix "R14" -> Just rest) = TokenRegister R14 : lexerRun rest
lexerRun (stripPrefix "R15" -> Just rest) = TokenRegister R15 : lexerRun rest
-- Number
lexerRun (stripPrefix "0b" -> Just rest) =
  case readBin rest of
    [(number, remaining)] -> TokenNumber number : lexerRun remaining
    _                     -> [TokenUnknown rest]
lexerRun (stripPrefix "0x" -> Just rest) =
  case readHex rest of
    [(number, remaining)] -> TokenNumber number : lexerRun remaining
    _                     -> [TokenUnknown rest]
lexerRun (stripPrefix "0o" -> Just rest) =
  case readOct rest of
    [(number, remaining)] -> TokenNumber number : lexerRun remaining
    _                     -> [TokenUnknown rest]
lexerRun (c:cs) | isDigit c =
  case readDec (c:cs) of
    [(number, remaining)] -> TokenNumber number : lexerRun remaining
    _                     -> [TokenUnknown (c:cs)]
-- Identifier
lexerRun (c:cs) | isAlpha c || c == '_' = TokenIdentifier identifier : lexerRun rest
  where (identifier, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs)
-- Unknown
lexerRun cs = [TokenUnknown cs]

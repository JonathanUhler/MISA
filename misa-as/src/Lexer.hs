{-# LANGUAGE ViewPatterns #-}


module Lexer (lexerRun, Token(..)) where


import Grammar

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import Numeric (readDec, readBin, readHex, readOct)


data Token
  = ColonToken
  | PeriodToken
  | OpcodeToken Opcode
  | RegisterToken Register
  | IdentifierToken String
  | NumberToken Int
  deriving (Show, Eq)


lexerGetColon :: String -> Maybe (Token, String)
lexerGetColon (':' : cs) = Just (ColonToken, cs)
lexerGetColon _          = Nothing


lexerGetPeriod :: String -> Maybe (Token, String)
lexerGetPeriod ('.' : cs) = Just (PeriodToken, cs)
lexerGetPeriod _          = Nothing


lexerGetOpcode :: String -> Maybe (Token, String)
lexerGetOpcode cs
  | Just rest <- stripPrefix "ADD"  cs = Just (OpcodeToken ADD,  rest)
  | Just rest <- stripPrefix "ADC"  cs = Just (OpcodeToken ADC,  rest)
  | Just rest <- stripPrefix "SUB"  cs = Just (OpcodeToken SUB,  rest)
  | Just rest <- stripPrefix "AND"  cs = Just (OpcodeToken AND,  rest)
  | Just rest <- stripPrefix "OR"   cs = Just (OpcodeToken OR,   rest)
  | Just rest <- stripPrefix "XOR"  cs = Just (OpcodeToken XOR,  rest)
  | Just rest <- stripPrefix "LW"   cs = Just (OpcodeToken LW,   rest)
  | Just rest <- stripPrefix "SW"   cs = Just (OpcodeToken SW,   rest)
  | Just rest <- stripPrefix "LA"   cs = Just (OpcodeToken LA,   rest)
  | Just rest <- stripPrefix "SA"   cs = Just (OpcodeToken SA,   rest)
  | Just rest <- stripPrefix "LI"   cs = Just (OpcodeToken LI,   rest)
  | Just rest <- stripPrefix "JLZ"  cs = Just (OpcodeToken JLZ,  rest)
  | Just rest <- stripPrefix "HALT" cs = Just (OpcodeToken HALT, rest)
  | otherwise                           = Nothing


lexerGetRegister :: String -> Maybe (Token, String)
lexerGetRegister ('R' : cs) =
    case readDec cs of
      [(r, remainder)] | minRegister <= r && r <= maxRegister ->
        Just (RegisterToken (toEnum r), drop (length cs - length remainder) cs)
      _                                                  -> Nothing
lexerGetRegister _ = Nothing


lexerGetIdentifier :: String -> Maybe (Token, String)
lexerGetIdentifier (c : cs) | isAlpha c || c == '_' =
  Just (IdentifierToken identifier, drop (length identifier) (c : cs))
  where (identifier, _) = span (\x -> isAlphaNum x || x == '_') (c : cs)
lexerGetIdentifier _ = Nothing


lexerGetNumber :: String -> Maybe (Token, String)
lexerGetNumber ('0' : 'b' : cs) =
  case readBin cs of
    [(number, remainder)] -> Just (NumberToken number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber ('0' : 'x' : cs) =
  case readHex cs of
    [(number, remainder)] -> Just (NumberToken number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber ('0' : 'o' : cs) =
  case readOct cs of
    [(number, remainder)] -> Just (NumberToken number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber cs =
  case readDec cs of
    [(number, remainder)] -> Just (NumberToken number, drop (length cs - length remainder) cs)
    _                     -> Nothing


lexerGetNextToken :: String -> Maybe (Token, String)
lexerGetNextToken cs
  =   lexerGetOpcode cs
  <|> lexerGetColon cs
  <|> lexerGetPeriod cs
  <|> lexerGetRegister cs
  <|> lexerGetIdentifier cs
  <|> lexerGetNumber cs


lexerRun :: String -> ([Token], String)
lexerRun "" = ([], "")
lexerRun (c:cs) | isSpace c = lexerRun rest
  where (_, rest) = span isSpace cs
lexerRun (';':cs) = lexerRun rest
  where (_, rest) = span (\x -> x /= '\n') cs
lexerRun cs =
  case lexerGetNextToken cs of
    Just (token, rest) -> (token : tokens, unlexed)
      where (tokens, unlexed) = lexerRun rest
    Nothing            -> ([], cs)

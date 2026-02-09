{-# LANGUAGE ViewPatterns #-}


module Lexer (lexerRun, Token(..)) where


import Grammar

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import Numeric (readDec, readBin, readHex, readOct)


data Token
  = TokenColon
  | TokenPeriod
  | TokenOpcode Opcode
  | TokenRegister Register
  | TokenIdentifier String
  | TokenNumber Int
  | TokenUnknown String
  deriving (Show, Eq)


lexerGetColon :: String -> Maybe (Token, String)
lexerGetColon (':' : cs) = Just (TokenColon, cs)
lexerGetColon _          = Nothing


lexerGetPeriod :: String -> Maybe (Token, String)
lexerGetPeriod ('.' : cs) = Just (TokenPeriod, cs)
lexerGetPeriod _          = Nothing


lexerGetOpcode :: String -> Maybe (Token, String)
lexerGetOpcode cs
  | Just rest <- stripPrefix "ADD"  cs = Just (TokenOpcode ADD,  rest)
  | Just rest <- stripPrefix "ADC"  cs = Just (TokenOpcode ADC,  rest)
  | Just rest <- stripPrefix "SUB"  cs = Just (TokenOpcode SUB,  rest)
  | Just rest <- stripPrefix "AND"  cs = Just (TokenOpcode AND,  rest)
  | Just rest <- stripPrefix "OR"   cs = Just (TokenOpcode OR,   rest)
  | Just rest <- stripPrefix "XOR"  cs = Just (TokenOpcode XOR,  rest)
  | Just rest <- stripPrefix "LW"   cs = Just (TokenOpcode LW,   rest)
  | Just rest <- stripPrefix "SW"   cs = Just (TokenOpcode SW,   rest)
  | Just rest <- stripPrefix "LA"   cs = Just (TokenOpcode LA,   rest)
  | Just rest <- stripPrefix "SA"   cs = Just (TokenOpcode SA,   rest)
  | Just rest <- stripPrefix "LI"   cs = Just (TokenOpcode LI,   rest)
  | Just rest <- stripPrefix "JLZ"  cs = Just (TokenOpcode JLZ,  rest)
  | Just rest <- stripPrefix "HALT" cs = Just (TokenOpcode HALT, rest)
  | otherwise                           = Nothing


lexerGetRegister :: String -> Maybe (Token, String)
lexerGetRegister ('R' : cs) =
    case readDec cs of
      [(r, remainder)] | minRegister <= r && r <= maxRegister ->
        Just (TokenRegister (toEnum r), drop (length cs - length remainder) cs)
      _                                                  -> Nothing
lexerGetRegister _ = Nothing


lexerGetIdentifier :: String -> Maybe (Token, String)
lexerGetIdentifier (c : cs) | isAlpha c || c == '_' =
  Just (TokenIdentifier identifier, drop (length identifier) (c : cs))
  where (identifier, _) = span (\x -> isAlphaNum x || x == '_') (c : cs)
lexerGetIdentifier _ = Nothing


lexerGetNumber :: String -> Maybe (Token, String)
lexerGetNumber ('0' : 'b' : cs) =
  case readBin cs of
    [(number, remainder)] -> Just (TokenNumber number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber ('0' : 'x' : cs) =
  case readHex cs of
    [(number, remainder)] -> Just (TokenNumber number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber ('0' : 'o' : cs) =
  case readOct cs of
    [(number, remainder)] -> Just (TokenNumber number, drop (length cs - length remainder) cs)
    _                     -> Nothing
lexerGetNumber cs =
  case readDec cs of
    [(number, remainder)] -> Just (TokenNumber number, drop (length cs - length remainder) cs)
    _                     -> Nothing


lexerGetNextToken :: String -> Maybe (Token, String)
lexerGetNextToken cs
  =   lexerGetOpcode cs
  <|> lexerGetColon cs
  <|> lexerGetPeriod cs
  <|> lexerGetRegister cs
  <|> lexerGetIdentifier cs
  <|> lexerGetNumber cs


lexerRun :: String -> [Token]
lexerRun "" = []
lexerRun (c:cs) | isSpace c = lexerRun rest
  where (_, rest) = span isSpace cs
lexerRun (';':cs) = lexerRun rest
  where (_, rest) = span (\x -> x /= '\n') cs
lexerRun cs =
  case lexerGetNextToken cs of
    Just (token, rest) -> token : lexerRun rest
    Nothing            -> [TokenUnknown cs]

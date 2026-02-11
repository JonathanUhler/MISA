{-# LANGUAGE ViewPatterns #-}


{- |
A simple lexer that processes a string of assembly code into a list of lexical tokens.

Author: Jonathan Uhler
-}
module Lexer (lexerRun, Token(..)) where


import Grammar

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import Numeric (readDec, readBin, readHex, readOct)


-- | The type of a lexical token.
data Token
  -- | The character ':'
  = ColonToken
  -- | The character '.'
  | PeriodToken
  -- | A string which is the name of a known instruction opcode.
  | OpcodeToken Opcode
  -- | A string which is the name of a known general-purpose register.
  | RegisterToken Register
  -- | A string which is not a register or opcode, of the form [_a-zA-Z][_a-zA-Z0-9]*
  | IdentifierToken String
  -- | A string which is an unsigned integer literal in dec, hex (0x), binary (0b), or octal (0o).
  | NumberToken Int
  deriving (Show, Eq)


{- |
Tries to parse a `ColonToken` from the head of the provided string.

If parsing succeeds, the colon token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
lexerGetColon :: String -> Maybe (Token, String)
lexerGetColon (':' : cs) = Just (ColonToken, cs)
lexerGetColon _          = Nothing


{- |
Tries to parse a `PeriodToken` from the head of the provided string.

If parsing succeeds, the period token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
lexerGetPeriod :: String -> Maybe (Token, String)
lexerGetPeriod ('.' : cs) = Just (PeriodToken, cs)
lexerGetPeriod _          = Nothing


{- |
Tries to parse an `OpcodeToken` from the head of the provided string.

If parsing succeeds, the opcode token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
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
  | Just rest <- stripPrefix "JZ"   cs = Just (OpcodeToken JZ,   rest)
  | Just rest <- stripPrefix "HALT" cs = Just (OpcodeToken HALT, rest)
  | otherwise                           = Nothing


{- |
Tries to parse a `RegisterToken` from the head of the provided string.

If parsing succeeds, the register token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
lexerGetRegister :: String -> Maybe (Token, String)
lexerGetRegister ('R' : cs) =
    case readDec cs of
      [(r, remainder)] | minRegister <= r && r <= maxRegister ->
        Just (RegisterToken (toEnum r), drop (length cs - length remainder) cs)
      _                                                  -> Nothing
lexerGetRegister _ = Nothing


{- |
Tries to parse an `IdentifierToken` from the head of the provided string.

Note that this function should be called in lexing after opcode and register identification,
since both of those are also valid identifiers (but are "reserved" identifiers, and thus lex as
opcodes/registers).

If parsing succeeds, the register token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
lexerGetIdentifier :: String -> Maybe (Token, String)
lexerGetIdentifier (c : cs) | isAlpha c || c == '_' =
  Just (IdentifierToken identifier, drop (length identifier) (c : cs))
  where (identifier, _) = span (\x -> isAlphaNum x || x == '_') (c : cs)
lexerGetIdentifier _ = Nothing


{- |
Tries to parse a `NumberToken` from the head of the provided string.

If parsing succeeds, the number token and the remainder of the input string with the token removed
are returned as a tuple. Otherwise `Nothing` is returned.
-}
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


{- |
Tries to parse any type of token from the head of the provided string.

The first token type that matches the head of the string will be returned, along with the
remainder of the input string as a tuple. If none of the lexical tokens match the head of the
input string, `Nothing` is returned.
-}
lexerGetNextToken :: String -> Maybe (Token, String)
lexerGetNextToken cs
  =   lexerGetOpcode cs
  <|> lexerGetColon cs
  <|> lexerGetPeriod cs
  <|> lexerGetRegister cs
  <|> lexerGetIdentifier cs
  <|> lexerGetNumber cs


{- |
Runs lexical analysis on the provided input string representing an assembly program.

The lexer returns a tuple containg the list of tokens parsed from the input string in the order
they appear, and a string which contains any unparsed input.

When a lexical error occurs (a part of the input string doesn't match any lexical token type),
the lexer will immediately stop and return the partially formed list of tokens and the remainder
of the input string, even if later parts of the unparsed string contain valid tokens.

The caller should check that the returned unparsed string is the empty string, which indicates that
the entire input was parsed successfully. If the string is non-empty, the lexical error occured
at the first character of the unparsed string.
-}
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

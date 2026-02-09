module Parser (parseProgram) where


import Architecture

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Word (Word8)
import Data.Void


type Parser = Parsec Void String


parseSkip :: Parser ()
parseSkip = L.space space1 (L.skipLineComment ";") empty


parseLexeme :: Parser a -> Parser a
parseLexeme = L.lexeme parseSkip


parseSymbol :: String -> Parser String
parseSymbol = L.symbol parseSkip


parseRegister :: Parser Register
parseRegister = parseLexeme $ do
  _ <- char 'R'
  n <- L.decimal
  if n >= minRegister && n <= maxRegister then
    return (toEnum n)
  else
    fail "Unknown register"


parseImmediate :: Parser Word8
parseImmediate = parseLexeme $ choice
  [string "0x" *> L.hexadecimal,
   string "0o" *> L.octal,
   string "0b" *> L.binary,
                  L.decimal]


parseInstruction :: Parser Instruction
parseInstruction = choice
  [ADD  <$ parseSymbol "ADD"  <*> parseRegister <*> parseRegister <*> parseRegister,
   ADC  <$ parseSymbol "ADC"  <*> parseRegister <*> parseRegister <*> parseRegister,
   SUB  <$ parseSymbol "SUB"  <*> parseRegister <*> parseRegister <*> parseRegister,
   AND  <$ parseSymbol "AND"  <*> parseRegister <*> parseRegister <*> parseRegister,
   OR   <$ parseSymbol "OR"   <*> parseRegister <*> parseRegister <*> parseRegister,
   XOR  <$ parseSymbol "XOR"  <*> parseRegister <*> parseRegister <*> parseRegister,
   LW   <$ parseSymbol "LW"   <*> parseRegister <*> parseImmediate,
   SW   <$ parseSymbol "SW"   <*> parseRegister <*> parseImmediate,
   LA   <$ parseSymbol "LA"   <*> parseRegister <*> parseRegister,
   SA   <$ parseSymbol "SA"   <*> parseRegister <*> parseRegister,
   LI   <$ parseSymbol "LI"   <*> parseRegister <*> parseImmediate,
   JLZ  <$ parseSymbol "JLZ"  <*> parseRegister <*> parseImmediate,
   HALT <$ parseSymbol "HALT" <*> parseImmediate]


parseProgram :: Parser [Instruction]
parseProgram = parseSkip *> many parseInstruction <* eof

module MemoryMap (MemMap, MemRegion(..), parseMemMap) where


import Data.Void
import Data.Word (Word16)
import Grammar
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


type MemMap = [MemRegion]


data MemRegion = MemRegion Word16 Word16 [Label]
  deriving Show


skip :: Parser ()
skip = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip


parseString :: String -> Parser String
parseString s = lexeme (string' s)


parseIdentifier :: Parser String
parseIdentifier = lexeme
  (
    do
      first <- letterChar <|> char '_'
      rest  <- many (alphaNumChar <|> char '_')
      return (first : rest)
  )


parseMemRegion :: Parser MemRegion
parseMemRegion = do
  _     <- parseString "section"
  start <- lexeme (string' "0x" *> L.hexadecimal)
  _     <- lexeme (char '-')
  end   <- lexeme (string' "0x" *> L.hexadecimal)
  _     <- lexeme (char ':')
  secs  <- many parseIdentifier <?> "section names"
  _     <- lexeme (char ';')
  return (MemRegion start end secs)


parseMemMap :: Parser MemMap
parseMemMap = skip *> some parseMemRegion <* eof

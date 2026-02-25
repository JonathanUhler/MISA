module MemoryMap (MemMap, MemRegion(..), parseMemMap, isOrphaned, isDuplicated, getRegion) where


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
parseMemMap = some parseMemRegion <* eof


numMappings :: Label -> MemMap -> Int
numMappings _ []    = 0
numMappings name ((MemRegion _ _ names) : regions)
  | elem name names = 1 + numMappings name regions
  | otherwise       =     numMappings name regions


isOrphaned :: Label -> MemMap -> Bool
isOrphaned name memmap = numMappings name memmap == 0


isDuplicated :: Label -> MemMap -> Bool
isDuplicated name memmap = numMappings name memmap > 1


getRegion :: Label -> MemMap -> Maybe MemRegion
getRegion _ []      = Nothing
getRegion name ((MemRegion start end names) : regions)
  | elem name names = Just (MemRegion start end names)
  | otherwise       = getRegion name regions

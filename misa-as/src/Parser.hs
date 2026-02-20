module Parser () where


import Data.Char
import qualified Data.Set as Set
import Data.Void
import Data.Word (Word8)
import Grammar
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


reservedOps :: [(String, Op)]
reservedOps = [(map toLower (show o), o) | o <- [minBound..maxBound :: Op]]

reservedGpRegs :: [(String, GpReg)]
reservedGpRegs = [(map toLower (show r), r) | r <- [minBound..maxBound :: GpReg]]

reservedWideRegs :: [(String, WideReg)]
reservedWideRegs = [(map toLower (show w), w) | w <- [minBound..maxBound :: WideReg]]

reservedCsrRegs :: [(String, CsrReg)]
reservedCsrRegs = [(map toLower (show c), c) | c <- [minBound..maxBound :: CsrReg]]

reservedCmpFlags :: [(String, CmpFlag)]
reservedCmpFlags = [(map toLower (show f), f) | f <- [minBound..maxBound :: CmpFlag]]

reservedDirs :: [String]
reservedDirs = ["word", "array", "section"]

reservedIdentifiers :: Set.Set String
reservedIdentifiers = Set.fromList (concat [map fst reservedOps,
                                            map fst reservedGpRegs,
                                            map fst reservedCsrRegs,
                                            map fst reservedCmpFlags,
                                            map fst reservedWideRegs,
                                            reservedDirs])


skip :: Parser ()
skip = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip


parseString :: String -> Parser String
parseString s = lexeme (string' s)


parseInteger :: Parser Int
parseInteger = lexeme (try parseHex <|> try parseBin <|> try parseOct <|> parseDec)
  where parseHex = parseString "0x" *> L.hexadecimal
        parseBin = parseString "0b" *> L.binary
        parseOct = parseString "0o" *> L.octal
        parseDec = L.decimal


parseWord :: Parser Word8
parseWord = do
  int <- parseInteger <?> "integer literal"
  if int < fromIntegral (minBound :: Word8) || int > fromIntegral (maxBound :: Word8) then
    fail ("integer literal " ++ show int ++ " is not a representable as a word")
  else
    return (fromIntegral int)


parseIdentifier :: Parser String
parseIdentifier = lexeme
  (
    do
      first <- letterChar <|> char '_'
      rest  <- many (alphaNumChar <|> char '_')
      return (first : rest)
  )


parseUnreservedIdentifier :: Parser String
parseUnreservedIdentifier = do
  ident <- parseIdentifier <?> "identifier"
  if Set.member (map toLower ident) reservedIdentifiers then
    fail ("reserved identifier '" ++ ident ++ "' cannot be used in this context")
  else
    return ident


parseLabel :: Parser Label
parseLabel = parseUnreservedIdentifier <* char ':'


parseDir :: Parser Dir
parseDir = do
  _   <- char '.'
  choice [WordDir    <$> (parseString "word"    *> parseWord),
          ArrayDir   <$> (parseString "array"   *> some parseWord),
          SectionDir <$> (parseString "section" *> parseUnreservedIdentifier)]


parseLookup :: [(String, a)] -> String -> Parser a
parseLookup env symbol = do
  ident <- parseIdentifier <?> symbol
  case lookup (map toLower ident) env of
    Just x  -> return x
    Nothing -> fail $ "unknown " ++ symbol ++ " '" ++ ident ++ "'"

parseGpReg :: Parser GpReg
parseGpReg = parseLookup reservedGpRegs "general purpose register"

parseCsrReg :: Parser CsrReg
parseCsrReg = parseLookup reservedCsrRegs "special register"

parseCmpFlag :: Parser CmpFlag
parseCmpFlag = parseLookup reservedCmpFlags "comparison flag"


parseLowImm :: Parser Imm
parseLowImm =
  choice [IntImm   Low . fromIntegral <$> parseWord <?> "integer literal",
          LabelImm Low                <$> parseUnreservedIdentifier <?> "label name"]


parseInst :: Parser Inst
parseInst =
  choice [AddInst  <$> (parseString "add"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          AdcInst  <$> (parseString "adc"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          SubInst  <$> (parseString "sub"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          SbbInst  <$> (parseString "sbb"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          AndInst  <$> (parseString "and"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          OrInst   <$> (parseString "or"   *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          XorInst  <$> (parseString "xor"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          RrcInst  <$> (parseString "rrc"  *> parseGpReg)   <*> parseGpReg,
          LwInst   <$> (parseString "lw"   *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          SwInst   <$> (parseString "sw"   *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
          RsrInst  <$> (parseString "rsr"  *> parseGpReg)   <*> parseGpReg <*> parseCsrReg,
          WsrInst  <$> (parseString "wsr"  *> parseGpReg)   <*> parseGpReg <*> parseCsrReg,
          SetInst  <$> (parseString "set"  *> parseGpReg)   <*> parseLowImm,
          JalInst  <$> (parseString "jal"  *> parseCmpFlag) <*> parseGpReg <*> parseGpReg,
          JmpInst  <$> (parseString "jmp"  *> parseCmpFlag) <*> parseGpReg <*> parseGpReg,
          HaltInst <$> (parseString "halt" *> parseGpReg)]


parseStatement :: Parser Statement
parseStatement =
  choice [InstStatement  <$> parseInst,
          DirStatement   <$> parseDir,
          LabelStatement <$> parseLabel]


parseProgram :: Parser Program
parseProgram = (some (lexeme parseStatement)) <* eof

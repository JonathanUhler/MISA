module Parser (parseProgram) where


import Grammar

import Data.Char
import qualified Data.Set as Set
import Data.Void
import Data.Word (Word8, Word16)
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

reservedAluFlags :: [(String, AluFlag)]
reservedAluFlags = [(map toLower (show f), f) | f <- [minBound..maxBound :: AluFlag]]

reservedCmpFlags :: [(String, CmpFlag)]
reservedCmpFlags = [(map toLower (show f), f) | f <- [minBound..maxBound :: CmpFlag]]

reservedDirs :: [String]
reservedDirs = ["word", "array", "section"]

reservedIdentifiers :: Set.Set String
reservedIdentifiers = Set.fromList (concat [map fst reservedOps,
                                            map fst reservedGpRegs,
                                            map fst reservedWideRegs,
                                            map fst reservedCsrRegs,
                                            map fst reservedAluFlags,
                                            map fst reservedCmpFlags,
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


parseDoubleWord :: Parser Word16
parseDoubleWord = do
  int <- parseInteger <?> "integer literal"
  if int < fromIntegral (minBound :: Word16) || int > fromIntegral (maxBound :: Word16) then
    fail ("integer literal " ++ show int ++ " is not a representable as a double-word")
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
parseLabel = (parseUnreservedIdentifier <?> "label") <* char ':'


parseDir :: Parser Dir
parseDir = do
  _   <- char '.' <?> "directive"
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

parseWideReg :: Parser (GpReg, GpReg)
parseWideReg = do
  wide <- parseLookup reservedWideRegs "register pair"
  case wide of
    RAB -> return (RA, RB)
    RCD -> return (RC, RD)
    REF -> return (RE, RF)
    RGH -> return (RG, RH)
    RUV -> return (RU, RV)
    RWX -> return (RW, RX)
    RYZ -> return (RY, RZ)

parseRegPair :: Parser (GpReg, GpReg)
parseRegPair = choice [try parseWideReg, (,) <$> parseGpReg <*> parseGpReg]

parseCsrReg :: Parser CsrReg
parseCsrReg = parseLookup reservedCsrRegs "special register"

parseAluFlag :: Parser AluFlag
parseAluFlag = parseLookup reservedAluFlags "alu flag"

parseCmpFlag :: Parser CmpFlag
parseCmpFlag = parseLookup reservedCmpFlags "comparison flag"


parseLowImm :: Parser Imm
parseLowImm =
  choice [IntImm   Low . fromIntegral <$> parseWord <?> "integer literal",
          LabelImm Low                <$> parseUnreservedIdentifier <?> "label name"]


parseFullImm :: Parser Imm
parseFullImm =
  choice [IntImm   Full . fromIntegral <$> parseDoubleWord <?> "integer literal",
          LabelImm Full                <$> parseUnreservedIdentifier <?> "label name"]


parseInst :: Parser Inst
parseInst = choice
  [
    -- Base instructions
    AddInst  <$> (parseString "add"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    AdcInst  <$> (parseString "adc"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    SubInst  <$> (parseString "sub"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    SbbInst  <$> (parseString "sbb"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    AndInst  <$> (parseString "and"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    OrInst   <$> (parseString "or"   *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    XorInst  <$> (parseString "xor"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    RrcInst  <$> (parseString "rrc"  *> parseGpReg)   <*> parseGpReg,
    (\rd (r1, r2) -> LwInst rd r1 r2) <$> (parseString "lw"   *> parseGpReg)   <*> parseRegPair,
    (\rd (r1, r2) -> SwInst rd r1 r2) <$> (parseString "sw"   *> parseGpReg)   <*> parseRegPair,
    (\c (r1, r2) -> RsrInst c r1 r2)  <$> (parseString "rsr"  *> parseCsrReg)  <*> parseRegPair,
    (\c (r1, r2) -> WsrInst c r1 r2)  <$> (parseString "wsr"  *> parseCsrReg)  <*> parseRegPair,
    SetInst  <$> (parseString "set"  *> parseGpReg)   <*> parseLowImm,
    (\f (r1, r2) -> JalInst f r1 r2)  <$> (parseString "jal"  *> parseAluFlag) <*> parseRegPair,
    (\f (r1, r2) -> JmpInst f r1 r2)  <$> (parseString "jmp"  *> parseAluFlag) <*> parseRegPair,
    HaltInst <$> (parseString "halt" *> parseGpReg),
    -- Pseudo instructions
    NopInst  <$   parseString "nop",
    NotInst  <$> (parseString "not" *> parseGpReg)   <*> parseGpReg,
    (\(r1, r2) i -> SetdInst r1 r2 i) <$> (parseString "setd" *> parseRegPair) <*> parseFullImm,
    CmpInst  <$> (parseString "cmp" *> parseCmpFlag) <*> parseGpReg <*> parseGpReg,
    TrueInst <$   parseString "true",
    (\(r1, r2) -> CallInst r1 r2)     <$> (parseString "call" *> parseRegPair),
    RetInst  <$   parseString "ret",
    ClrInst  <$> (parseString "clr" *> parseAluFlag)
  ] <?> "instruction"


parseStatement :: Parser Statement
parseStatement =
  choice [InstStatement  <$> parseInst,
          DirStatement   <$> parseDir,
          LabelStatement <$> parseLabel]


parseProgram :: Parser Program
parseProgram = (some (skip *> parseStatement)) <* eof

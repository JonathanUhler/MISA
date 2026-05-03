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

reservedCmpFlags :: [(String, CmpFlag)]
reservedCmpFlags = [(map toLower (show f), f) | f <- [minBound..maxBound :: CmpFlag]]

reservedDirs :: [String]
reservedDirs = ["word", "array", "section"]

reservedIdentifiers :: Set.Set String
reservedIdentifiers = Set.fromList (concat [map fst reservedOps,
                                            map fst reservedGpRegs,
                                            map fst reservedWideRegs,
                                            map fst reservedCsrRegs,
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


parseThisIdent :: String -> Parser String
parseThisIdent expected = try $ do
  actual <- parseIdentifier
  if map toLower actual == map toLower expected then
    return expected
  else
    fail ("unexpected identifier")


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
    RAB      -> return (RA, RB)
    RCD      -> return (RC, RD)
    REF      -> return (RE, RF)
    RUV      -> return (RU, RV)
    RWX      -> return (RW, RX)
    RYZ      -> return (RY, RZ)
    RSCRATCH -> return (RSCRATCH0, RSCRATCH1)

parseRegPair :: Parser (GpReg, GpReg)
parseRegPair = choice [try parseWideReg, (,) <$> parseGpReg <*> parseGpReg]

parseCsrReg :: Parser CsrReg
parseCsrReg = parseLookup reservedCsrRegs "special register"

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
    AddInst  <$> (parseThisIdent "add"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    AdcInst  <$> (parseThisIdent "adc"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    SubInst  <$> (parseThisIdent "sub"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    SbbInst  <$> (parseThisIdent "sbb"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    AndInst  <$> (parseThisIdent "and"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    OrInst   <$> (parseThisIdent "or"   *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    XorInst  <$> (parseThisIdent "xor"  *> parseGpReg)   <*> parseGpReg <*> parseGpReg,
    RrcInst  <$> (parseThisIdent "rrc"  *> parseGpReg)   <*> parseGpReg,
    (\rd (r1, r2) -> LwInst rd r1 r2) <$> (parseThisIdent "lw"   *> parseGpReg)   <*> parseRegPair,
    (\rd (r1, r2) -> SwInst rd r1 r2) <$> (parseThisIdent "sw"   *> parseGpReg)   <*> parseRegPair,
    (\c (r1, r2) -> RsrInst c r1 r2)  <$> (parseThisIdent "rsr"  *> parseCsrReg)  <*> parseRegPair,
    (\c (r1, r2) -> WsrInst c r1 r2)  <$> (parseThisIdent "wsr"  *> parseCsrReg)  <*> parseRegPair,
    SetInst  <$> (parseThisIdent "set"  *> parseGpReg)   <*> parseLowImm,
    (\f (r1, r2) -> JalInst f r1 r2)  <$> (parseThisIdent "jal"  *> parseCmpFlag) <*> parseRegPair,
    (\f (r1, r2) -> JmpInst f r1 r2)  <$> (parseThisIdent "jmp"  *> parseCmpFlag) <*> parseRegPair,
    HaltInst <$> (parseThisIdent "halt" *> parseGpReg),
    -- Pseudo instructions
    NopInst    <$   parseThisIdent "nop",
    MovInst    <$> (parseThisIdent "mov" *> parseGpReg) <*> parseGpReg,
    CmpInst    <$> (parseThisIdent "cmp" *> parseGpReg) <*> parseGpReg,
    (\(r1, r2) i -> Set2Inst r1 r2 i) <$> (parseThisIdent "set2" *> parseRegPair) <*> parseFullImm,
    (\f i -> JmpiInst f i)            <$> (parseThisIdent "jmpi" *> parseCmpFlag) <*> parseFullImm,
    (\f i -> JaliInst f i)            <$> (parseThisIdent "jali" *> parseCmpFlag) <*> parseFullImm,
    GotoInst   <$> (parseThisIdent "goto" *> parseFullImm),
    CallInst   <$> (parseThisIdent "call" *> parseFullImm),
    RetInst    <$   parseThisIdent "ret",
    ClrInst    <$   parseThisIdent "clr",
    PushInst   <$> (parseThisIdent "push" *> parseGpReg),
    PopInst    <$> (parseThisIdent "pop" *> parseGpReg)
  ] <?> "instruction"


parseStatement :: Parser Statement
parseStatement =
  choice [InstStatement  <$> parseInst,
          DirStatement   <$> parseDir,
          LabelStatement <$> parseLabel]


parseProgram :: Parser Program
parseProgram = (some (skip *> parseStatement <* skip)) <* eof

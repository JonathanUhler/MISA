module MacroProcessor (resolveMacros) where


import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String


type MacroMap = [(String, String)]


data Line
  = Macro String String
  | SourceLine String
  deriving (Show)


parseIdentifier :: Parser String
parseIdentifier = do
  _ <- hspace
  first <- letterChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_')
  return (first : rest)


parseMacro :: Parser Line
parseMacro = do
  _   <- space
  _   <- string "#macro"
  _   <- space1
  key <- parseIdentifier
  _   <- space1
  val <- someTill anySingle (string "#endm")
  return (Macro key val)


parseSourceLine :: Parser Line
parseSourceLine = do
  notFollowedBy eof
  content <- manyTill anySingle (eol <|> (eof >> return ""))
  return (SourceLine content)


parseLine :: Parser Line
parseLine = try parseMacro <|> parseSourceLine


parseLines :: Parser [Line]
parseLines = many parseLine <* eof


isMacroLine :: Line -> Bool
isMacroLine (Macro _ _) = True
isMacroLine _           = False


getMacroMap :: [Line] -> MacroMap
getMacroMap ls = map (\(Macro k v) -> (k, v)) macroLines
  where macroLines = filter isMacroLine ls


getSource :: [Line] -> String
getSource ls = unlines (map (\(SourceLine s) -> s) sourceLines)
  where sourceLines = filter (not . isMacroLine) ls


findAndReplaceOne :: (String, String) -> String -> String
findAndReplaceOne (k, v) = concatMap replaceMacro . tokenizeSource
  where tokenizeSource = groupBy ((==) `on` isSpace)
        replaceMacro sourceToken
          | sourceToken == k = v
          | otherwise        = sourceToken


findAndReplaceAll :: MacroMap -> String -> String
findAndReplaceAll []                source = source
findAndReplaceAll ((k, v) : macros) source = findAndReplaceAll macros newSource
  where newSource = findAndReplaceOne (k, v) source


resolveMacros :: String -> Either String String
resolveMacros content =
  case parse parseLines "<input>" content of
    Left err -> Left (errorBundlePretty err)
    Right ls -> Right (findAndReplaceAll macros source)
      where macros = getMacroMap ls
            source = getSource ls

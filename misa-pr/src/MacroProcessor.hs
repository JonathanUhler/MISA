module MacroProcessor (resolveMacros) where


import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String


type MacroMap = [(String, String)]


data Chunk
  = Macro String String
  | SourceChunk String
  deriving (Show)


parseIdentifier :: Parser String
parseIdentifier = do
  _ <- hspace
  first <- letterChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_')
  return (first : rest)


parseMacro :: Parser Chunk
parseMacro = do
  _   <- space
  _   <- string "#macro"
  _   <- space1
  key <- parseIdentifier
  _   <- space1
  val <- someTill anySingle (string "#endm")
  return (Macro key val)


parseSourceChunk :: Parser Chunk
parseSourceChunk = do
  content <- some (notFollowedBy parseMacro *> anySingle)
  return (SourceChunk content)


parseChunks :: Parser [Chunk]
parseChunks = many (try parseMacro <|> parseSourceChunk) <* eof


replaceMacro :: String -> (String, String) -> String
replaceMacro content (k, v) = (concatMap filterToken . tokenizeSource) content
  where tokenizeSource = groupBy ((==) `on` isSpace)
        filterToken sourceToken
          | sourceToken == k = v
          | otherwise        = sourceToken


replaceMacros :: MacroMap -> String -> String
replaceMacros macros content = foldl replaceMacro content macros


resolveChunks :: [Chunk] -> String
resolveChunks chunks = acc [] chunks
  where acc :: MacroMap -> [Chunk] -> String
        acc _      []                       = ""
        acc macros ((Macro k v) : rest)     = acc ((k, v) : macros) rest
        acc macros ((SourceChunk s) : rest) = (replaceMacros macros s) ++ (acc macros rest)


resolveMacros :: String -> Either String String
resolveMacros content =
  case parse parseChunks "<input>" content of
    Left err     -> Left (errorBundlePretty err)
    Right chunks -> Right (resolveChunks chunks)

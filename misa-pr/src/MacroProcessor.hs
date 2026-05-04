module MacroProcessor (MacroMap, resolveMacros) where


import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String


type MacroMap = [(String, [String], String)]


data Chunk
  = Macro String String
  | PolyMacro String [String] String
  | SourceChunk String
  deriving (Show)


parseIdentifier :: Parser String
parseIdentifier = do
  _ <- hspace
  first <- letterChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_')
  return (first : rest)


parsePolyMacro :: Parser Chunk
parsePolyMacro = do
  _    <- space
  _    <- string "#macro"
  _    <- space1
  key  <- parseIdentifier
  _    <- string "("
  args <- some (space *> parseIdentifier <* space)
  _    <- string ")"
  _    <- space1
  val  <- someTill anySingle (string "#endm")
  return (PolyMacro key args val)


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
parseChunks = many (try parsePolyMacro <|> try parseMacro <|> parseSourceChunk) <* eof


replaceMacro :: String -> (String, [String], String) -> String
replaceMacro content (k, [], v) = (concatMap filterToken . groupBy ((==) `on` isSpace)) content
  where filterToken t
          | t == k    = v
          | otherwise = t
replaceMacro content (k, a, v) = scanSource content
  where scanSource [] = []
        scanSource s@(c : cs)
          | (k ++ "(") `isPrefixOf` s =
            let afterOpen = drop (length k + 1) s
            in case span (/= ')') afterOpen of
                 (inside, ')':remaining) ->
                   let args = words inside
                   in if length args == length a
                      then substituteArgs (zip a args) v ++ scanSource remaining
                      else c : scanSource cs
                 _ -> c : scanSource cs
          | otherwise = c : scanSource cs
        substituteArgs mapping = concatMap subToken . groupBy ((==) `on` isSpace)
          where subToken t = fromMaybe t (lookup t mapping)


replaceMacros :: MacroMap -> String -> String
replaceMacros macros content = foldl replaceMacro content macros


resolveChunks :: [Chunk] -> MacroMap -> String
resolveChunks chunks macroDefs = acc macroDefs chunks
  where acc :: MacroMap -> [Chunk] -> String
        acc _      []                         = ""
        acc macros ((Macro k v) : rest)       = acc ((k, [], v) : macros) rest
        acc macros ((PolyMacro k a v) : rest) = acc ((k, a, v) : macros) rest
        acc macros ((SourceChunk s) : rest)   = (replaceMacros macros s) ++ (acc macros rest)


resolveMacros :: String -> MacroMap -> Either String String
resolveMacros content macroDefs =
  case parse parseChunks "<input>" content of
    Left err     -> Left (errorBundlePretty err)
    Right chunks -> Right (resolveChunks chunks macroDefs)

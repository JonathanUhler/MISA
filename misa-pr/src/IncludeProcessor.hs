module IncludeProcessor (resolveInclusions) where


import Control.Exception (catch, IOException)
import Control.Monad (forM)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char


data Line
  = Include FilePath
  | SourceLine String
  deriving (Show)


type Parser = Parsec Void String


parseInclude :: Parser Line
parseInclude = do
  notFollowedBy eof
  _    <- hspace
  _    <- string "#include"
  _    <- hspace1
  path <- char '"' *> someTill anySingle (char '"')
  return (Include path)


parseSourceLine :: Parser Line
parseSourceLine = do
  notFollowedBy eof
  content <- manyTill anySingle (eol <|> (eof >> return ""))
  return (SourceLine content)


parseLines :: Parser [Line]
parseLines = many (try parseInclude <|> parseSourceLine) <* eof


resolveLine :: Line -> IO (Either String [String])
resolveLine (SourceLine s) = return (Right [s])
resolveLine (Include path) = do
  result <- readFileWithIncludePaths path

  case result of
    Left err      -> return (Left err)
    Right content -> do
      resolved <- resolveInclusions content

      case resolved of
        Left err   -> return (Left err)
        Right good -> return (Right (lines good))


resolveLines :: [Line] -> IO (Either String [String])
resolveLines ls = do
  results <- forM ls resolveLine
  return (fmap concat (sequence results))


readFileWithIncludePaths :: FilePath -> IO (Either String String)
readFileWithIncludePaths filename = do
  includePaths <- getIncludePaths
  let candidates = filename : map (</> filename) includePaths
  tryPaths candidates
  where
    tryPaths [] = return $ Left ("Could not find file '" <> filename <> "' in any include path")
    tryPaths (p : ps) = do
      result <- readFileSafe p
      case result of
        Right contents -> return (Right contents)
        Left _         -> tryPaths ps


readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
  result <- (Right <$> readFile path) `catch` handler
  return result
  where handler :: IOException -> IO (Either String String)
        handler e = return (Left ("Could not read included file '" <> path <> "': " <> show e))


resolveInclusions :: String -> IO (Either String String)
resolveInclusions content =
  case parse parseLines "<input>" content of
    Left err -> return (Left (errorBundlePretty err))
    Right ls -> fmap (fmap unlines) (resolveLines ls)


getIncludePaths :: IO [FilePath]
getIncludePaths = do
  mval <- lookupEnv "MISA_PR_INCLUDE_PATH"
  let val = fromMaybe "" mval
  return $ filter (not . null) $ splitOn ":" val

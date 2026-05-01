module IncludeProcessor (resolveInclusions) where


import Control.Exception (catch, IOException)
import Control.Monad (forM)
import Data.Void (Void)
import System.FilePath (takeDirectory, (</>))
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
  path <- someTill anySingle (eol <|> (eof >> return ""))
  return (Include path)


parseSourceLine :: Parser Line
parseSourceLine = do
  notFollowedBy eof
  content <- manyTill anySingle (eol <|> (eof >> return ""))
  return (SourceLine content)


parseLine :: Parser Line
parseLine = try parseInclude <|> parseSourceLine


parseLines :: Parser [Line]
parseLines = many parseLine <* eof


resolveLine :: FilePath -> Line -> IO (Either String [String])
resolveLine _          (SourceLine s) = return (Right [s])
resolveLine currentDir (Include path) = do
  let resolvedPath = currentDir </> path
  result <- readFileSafe resolvedPath

  case result of
    Left err      -> return (Left err)
    Right content -> do
      let includedDir = takeDirectory resolvedPath
      resolved <- resolveInclusions includedDir content

      case resolved of
        Left err   -> return (Left err)
        Right good -> return (Right (lines good))


resolveLines :: FilePath -> [Line] -> IO (Either String [String])
resolveLines currentDir ls = do
  results <- forM ls (resolveLine currentDir)
  return (fmap concat (sequence results))


readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
  result <- (Right <$> readFile path) `catch` handler
  return result
  where handler :: IOException -> IO (Either String String)
        handler e = return (Left ("Could not read included file '" <> path <> "': " <> show e))


resolveInclusions :: FilePath -> String -> IO (Either String String)
resolveInclusions currentDir content =
  case parse parseLines "<input>" content of
    Left err -> return (Left (errorBundlePretty err))
    Right ls -> fmap (fmap unlines) (resolveLines currentDir ls)

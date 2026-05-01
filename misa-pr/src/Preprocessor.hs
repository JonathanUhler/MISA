module Preprocessor (preprocessFile) where


import IncludeProcessor
import MacroProcessor

import System.FilePath


preprocessFile :: FilePath -> IO (Either String String)
preprocessFile path = do
  content       <- readFile path
  includeResult <- resolveInclusions (takeDirectory path) content
  case includeResult of
    Left err           -> return (Left err)
    Right totalContent -> return (resolveMacros totalContent)

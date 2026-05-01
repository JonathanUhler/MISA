module Main (main) where


import Preprocessor

import System.Environment (getArgs)
import System.Exit
import System.IO


process :: FilePath -> FilePath -> IO ()
process inputPath outputPath = do
  processResult <- preprocessFile inputPath
  case processResult of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right processedContent -> do
      writeFile outputPath processedContent


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath] -> process inputPath outputPath
    _                       -> exitWith (ExitFailure 1)

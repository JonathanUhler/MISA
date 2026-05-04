module Main (main) where


import MacroProcessor
import Preprocessor

import System.Environment (getArgs)
import System.Exit
import System.IO


process :: FilePath -> FilePath -> MacroMap -> IO ()
process inputPath outputPath macroDefs = do
  processResult <- preprocessFile inputPath macroDefs
  case processResult of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right processedContent -> do
      writeFile outputPath processedContent


formatDefs :: [String] -> IO MacroMap
formatDefs []             = return []
formatDefs (k : v : rest) = do
  otherDefs <- formatDefs rest
  return ((k, [], v) : otherDefs)
formatDefs k              = do
  hPutStrLn stderr ("no value associated with macro definition '" ++ show k ++ "'")
  exitWith (ExitFailure 1)


main :: IO ()
main = do
  args <- getArgs
  case args of
    inputPath : outputPath : rest -> do
      macroDefs <- formatDefs rest
      process inputPath outputPath macroDefs
    _                             -> exitWith (ExitFailure 1)

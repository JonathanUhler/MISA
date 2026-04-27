module Main (main) where


import Names

import qualified Data.ByteString as B
import ObjectFile
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Megaparsec


listNames :: FilePath -> IO ()
listNames inputPath = do
  content <- B.readFile inputPath
  case runParser unpackBinaryObject inputPath content of
    Left  err -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right obj -> do
      putStrLn ("File " ++ (show inputPath) ++ "\n")
      let names = unlines (map ("  " ++) (lines (formatNames obj)))
      putStrLn names


main :: IO ()
main = do
  args <- getArgs
  case args of
    (objPath : []) -> do
      listNames objPath
    _              -> exitWith (ExitFailure 1)

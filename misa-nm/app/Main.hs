module Main (main) where


import Names

import qualified Data.ByteString as B
import ObjectFile
import System.Environment (getArgs)
import Text.Megaparsec


listNames :: FilePath -> IO ()
listNames inputPath = do
  content <- B.readFile inputPath
  case runParser unpackBinaryObject inputPath content of
    Left  err -> putStrLn (errorBundlePretty err)
    Right obj -> do
      let names = formatNames obj
      putStrLn names


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> listNames inputPath
    _           -> putStrLn "usage: misa-nm <inputPath>"

module Main (main) where


import Encoder
import ObjectFile
import Parser

import qualified Data.ByteString as B
import System.Environment (getArgs)
import Text.Megaparsec


assemble :: FilePath -> FilePath -> IO ()
assemble inputPath outputPath = do
  content <- readFile inputPath
  case runParser parseProgram inputPath content of
    Left  err     -> putStrLn (errorBundlePretty err)
    Right program -> do
      let object = encodeProgram program
      let bytes  = packBinaryObject object
      B.writeFile outputPath (B.pack bytes)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath] -> assemble inputPath outputPath
    _                       -> putStrLn "usage: misa-as <inputPath> <outputPath>"

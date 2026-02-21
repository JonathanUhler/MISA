module Main (main) where


--import Encoder
--import ObjectFile
import Parser

import qualified Data.ByteString as B
import System.Environment (getArgs)
import Text.Megaparsec


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath] -> do
      content <- readFile inputPath
      case runParser parseProgram inputPath content of
        Left  err     -> putStrLn (errorBundlePretty err)
        Right program -> print program
    _                       -> putStrLn "usage: misa-as <inputPath> <outputPath>"
--      bytes   <- packBinaryObject object
--      B.writeFile outputPath (B.pack bytes)

module Main (main) where


import Encoder (encodeProgram)
import Parser (parseProgram)

import Text.Megaparsec (runParser, errorBundlePretty)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.ByteString as B


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> assemble inputFile outputFile
    _                       -> putStrLn "Usage: misa-as <input.asm> <output.bin>"


assemble :: FilePath -> FilePath -> IO ()
assemble inputFile outputFile = do
  content <- readFile inputFile
  case runParser parseProgram inputFile content of
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitFailure
    Right instructions -> do
      B.writeFile outputFile (B.pack (encodeProgram instructions))

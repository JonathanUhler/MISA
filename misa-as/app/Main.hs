module Main (main) where


import Encoder
import Lexer
import ObjectFile
import Parser

import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.ByteString as B


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath] -> do
      content <- readFile inputPath

      let (tokens, unlexed) = lexerRun content
      if unlexed /= ""
        then die ("Lexical Error: Unexpected input at: " ++ take 10 unlexed ++ "...")
        else return ()

      let (program, unparsed) = parserRun tokens
      if unparsed /= []
        then die ("Semantic Error: Unexpected input at: " ++ show (take 10 unparsed) ++ "... got " ++ show program)
        else return ()

      let object = encoderRun program
      let bytes = packBinaryObject object
      B.writeFile outputPath (B.pack bytes)

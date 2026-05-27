module Main (main) where


import Encoder
import Grammar
import ObjectFile
import Parser

import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as B
import Data.Word (Word16)
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Megaparsec


assemble :: [Extn] -> FilePath -> FilePath -> IO ()
assemble extns inputPath outputPath = do
  content <- readFile inputPath
  case runParser (parseProgram extns) inputPath content of
    Left  err     -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right program -> do
      let object = encodeProgram program
      let bytes  = packBinaryObject object
      B.writeFile outputPath (B.pack bytes)


getExtns :: Word16 -> [Extn]
getExtns extnBits = foldr filterExtn [] [0..(length allExtns - 1)]
  where filterExtn bitNum extns
          | (shiftR extnBits bitNum) .&. 0x1 == 0x1 = (allExtns !! bitNum) : extns
          | otherwise                             =                        extns


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPath, extnBitStr] -> assemble (getExtns extnBits) inputPath outputPath
      where extnBits = (read extnBitStr :: Word16)
    _                                   -> exitWith (ExitFailure 1)

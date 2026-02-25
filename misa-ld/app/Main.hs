module Main (main) where


import Linker
import MemoryMap

import qualified Data.ByteString as B
import ObjectFile
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec


link :: FilePath -> [FilePath] -> FilePath -> IO ()
link scriptPath objPaths outputPath = do
  rawObjs <- mapM B.readFile objPaths
  let objResult = traverse (\(p, c) -> runParser unpackBinaryObject p c) (zip objPaths rawObjs)
    
  binObjects <- case objResult of
    Left err -> die $ "Object Parse Error:\n" ++ errorBundlePretty err
    Right os -> return os

  scriptContent <- readFile scriptPath
  let scriptResult = runParser parseMemMap scriptPath scriptContent
    
  memMap <- case scriptResult of
    Left err -> die $ "Script Parse Error:\n" ++ errorBundlePretty err
    Right m  -> return m

  case linkBinaryObjects binObjects memMap of
    Left err    -> die $ "Linker Error: " ++ show err
    Right bytes -> B.writeFile outputPath (B.pack bytes)


main :: IO ()
main = do
  args <- getArgs
  case args of
    (scriptPath : rest)
      | not (null rest) -> do
          let objPaths = init rest
          let outputPath = last rest
          link scriptPath objPaths outputPath
    _                       -> putStrLn "usage: misa-ld <scriptPath> <objPath...> <outputPath>"

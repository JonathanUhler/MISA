module Main (main) where


import Archiver

import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Megaparsec


createArchive :: [FilePath] -> FilePath -> IO ()
createArchive objPaths outputPath = do
  archive <- loadNamedBinaryObjects objPaths
  B.writeFile outputPath (B.pack (packBinaryArchive archive))


extractArchive :: FilePath -> FilePath -> IO ()
extractArchive archivePath outputPath = do
  rawArchive <- B.readFile archivePath
  let archiveResult = runParser unpackBinaryArchive archivePath rawArchive

  archive <- case archiveResult of
    Left err -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right ar -> return ar

  saveNamedBinaryObjects archive outputPath


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("c" : rest) | not (null rest)        -> do
      let objPaths = init rest
      let outputPath = last rest
      createArchive objPaths outputPath
    ("x" : archivePath : outputPath : []) -> do
      extractArchive archivePath outputPath
    _ -> exitWith (ExitFailure 1)
                     

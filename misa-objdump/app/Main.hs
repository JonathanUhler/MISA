module Main (main) where


import Decoder
import Printer
import Rewriter


import qualified Data.ByteString as B
import ObjectFile
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Megaparsec


decodeObject :: FilePath -> Bool -> IO ()
decodeObject objPath decodeAll = do
  rawObj    <- B.readFile objPath
  binObject <- case runParser unpackBinaryObject objPath rawObj of
    Left err  -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right obj -> return obj

  let rawBinary = concatMap (\(Sec _ [LiteralCode binary] _ _) -> binary) binObject
  let rawProg   = decodeBinaryObject binObject decodeAll
  let prog      = rewriteProgram rawProg
  let strProg   = printProgram rawBinary prog

  putStrLn strProg


decodeBinary :: FilePath -> FilePath -> IO ()
decodeBinary binPath symsObjPath = do
  rawBytes <- B.readFile binPath
  let rawBinary = B.unpack rawBytes

  rawSymsObj <- B.readFile symsObjPath
  symsObject <- case runParser unpackBinaryObject symsObjPath rawSymsObj of
    Left err  -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right obj -> return obj

  (syms, relocs) <- case symsObject of
    []                          -> return ([], [])
    ((Sec _ _ syms relocs) : _) -> return (syms, relocs)

  let rawProg = decodeFlatBinary rawBinary syms relocs
  let prog    = rewriteProgram rawProg
  let strProg = printProgram rawBinary prog

  putStrLn strProg


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["misa_lf", objPath, decodeAll]   -> decodeObject objPath (read decodeAll)
    ["binary",  binPath, symsObjPath] -> decodeBinary binPath symsObjPath
    _                                 -> exitWith (ExitFailure 1)

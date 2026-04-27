module Archiver (BinaryArchive,
                 loadNamedBinaryObjects,
                 saveNamedBinaryObjects,
                 packBinaryArchive,
                 unpackBinaryArchive) where


import ObjectFile
import Packing

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Void
import Data.Word (Word8)
import System.Exit
import System.FilePath
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Byte


type Parser = Parsec Void B.ByteString


type BinaryArchive = [NamedBinaryObject]


magicHeader :: String
magicHeader = "MISA_AR "


loadNamedBinaryObjects :: [FilePath] -> IO [NamedBinaryObject]
loadNamedBinaryObjects objPaths = do
  rawObjs <- mapM B.readFile objPaths
  let objResult = traverse (\(p, c) -> runParser unpackBinaryObject p c) (zip objPaths rawObjs)

  binObjs <- case objResult of
    Left err -> do
      hPutStrLn stderr (errorBundlePretty err)
      exitWith (ExitFailure 1)
    Right os -> return os

  namedObjs <-
    mapM (\(p, o) -> return (NamedBinaryObject p o)) (zip (map takeFileName objPaths) binObjs)
  return namedObjs


saveNamedBinaryObjects :: [NamedBinaryObject] -> FilePath -> IO ()
saveNamedBinaryObjects namedObjs outputPath = mapM_ saveNamedBinaryObject namedObjs
  where saveNamedBinaryObject :: NamedBinaryObject -> IO ()
        saveNamedBinaryObject (NamedBinaryObject path obj) =
          B.writeFile (outputPath </> path) (B.pack (packBinaryObject obj))


packBinaryArchive :: BinaryArchive -> [Word8]
packBinaryArchive [] = packDoubleWord 0
packBinaryArchive objs
  =  B.unpack (E.encodeUtf8 (T.pack magicHeader))
  ++ packDoubleWord (fromIntegral (length objs))
  ++ packedObjs
  where packedObjs = concatMap packNamedBinaryObject objs


unpackBinaryArchive :: Parser BinaryArchive
unpackBinaryArchive = do
  _       <- string (E.encodeUtf8 (T.pack magicHeader))
  numObjs <- unpackDoubleWord
  objs    <- count (fromIntegral numObjs) unpackNamedBinaryObject
  return objs

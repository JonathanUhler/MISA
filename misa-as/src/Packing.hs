module Packing (packString, packDoubleWord, unpackString, unpackDoubleWord) where


import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Void
import Data.Word (Word8, Word16)


import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary


type Parser = Parsec Void B.ByteString


{- |
Packs an ASCII string into a list of bytes.

The returned list begins with a 2-byte doubleword in little-endian format which is the number of
bytes in the following string. The list then contains all the bytes of the string using UTF8
encoding and little-endian order (first character of the string at lowest address).
-}
packString :: String -> [Word8]
packString str
  = packDoubleWord (fromIntegral (length packedString)) ++ packedString
  where packedString = B.unpack (E.encodeUtf8 (T.pack str))


{- |
Packs a 16-bit word into two 8-bit words.

The returned list is in little-endian format.
-}
packDoubleWord :: Word16 -> [Word8]
packDoubleWord dword = [fromIntegral (dword .&. 0x00FF), fromIntegral (shiftR dword 8)]


unpackString :: Parser String
unpackString = do
  len   <- unpackDoubleWord
  bytes <- count (fromIntegral len) asciiChar
  return (map (chr . fromIntegral) bytes)


unpackDoubleWord :: Parser Word16
unpackDoubleWord = word16le

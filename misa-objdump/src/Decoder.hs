module Decoder (decodeBinaryObject) where


import Data.Word (Word8)
import Grammar
import ObjectFile


decodeSec :: Sec -> Program
decodeSec (Sec _ [LiteralCode binary] syms _) = undefined
decodeSec (Sec _ code syms _)                 = undefined


decodeBinaryObject :: BinaryObject -> Bool -> Program
decodeBinaryObject secs decodeAll = concatMap decodeSec secsToDecode
  where secsToDecode = filter (\(Sec name _ _ _) -> name == "text" || decodeAll) secs

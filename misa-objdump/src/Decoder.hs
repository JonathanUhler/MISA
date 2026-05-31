module Decoder (decodeBinary, decodeBinaryObject) where


import Data.Word (Word8)
import Grammar
import ObjectFile


decodeBinary :: Label -> [Word8] -> SymTable -> Program
decodeBinary name binary syms = decodeBinaryObject object
  where object = [Sec name [LiteralCode binary] syms []]


decodeBinaryObject :: BinaryObject -> Program
decodeBinaryObject object = undefined

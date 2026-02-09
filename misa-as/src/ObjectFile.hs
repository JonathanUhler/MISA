module ObjectFile () where


import Data.Word (Word8, Word16)


data Symbol = Symbol String Word16


data Relocation = Relocation String Word16


data Object = Object [Word8] [Symbol] [Relocation]

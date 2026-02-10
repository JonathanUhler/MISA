module ObjectFile (makeObject) where


import Grammar

import Data.Word (Word8, Word16)


data BinaryObject = BinaryObject Code SymbolTable RelocationTable
  deriving Show


type Code = [Word8]


type SymbolTable = [Symbol]


data Symbol = Symbol Label Word16
  deriving Show


type RelocationTable = [Relocation]


data Relocation = Relocation RelocationType Word16 Label
  deriving Show


data RelocationType = FullRelocation | HighRelocation | LowRelocation
  deriving (Show, Enum)


makeObject :: Program -> BinaryObject
makeObject [] = BinaryObject [] [] []

module Relocator (applyRelocs, getAllSyms) where


import Placer

import Data.Bits ((.&.), shiftR)
import ObjectFile


applyRelocs :: [PlacedSec] -> SymTable -> [PlacedSec]
applyRelocs [] _              = []
applyRelocs (sec : secs) syms = relocSec sec syms : applyRelocs secs syms


relocSec :: PlacedSec -> SymTable -> PlacedSec
relocSec (PlacedSec (Sec name code secSyms relocs) place) syms
  = PlacedSec (Sec name newCode secSyms []) place
  where newCode = foldr (relocSymInCode syms) code relocs


relocSymInCode :: SymTable -> Reloc -> Code -> Code
relocSymInCode (Sym sym value : syms) (Reloc kind addr reloc) (LiteralCode arr : [])
  | sym == reloc = [LiteralCode (front ++ [fromIntegral middle] ++ back)]
  | otherwise    = relocSymInCode syms (Reloc kind addr reloc) [LiteralCode arr] 
  where (front, rest) = splitAt (fromIntegral addr) arr
        middle        = if kind == LowReloc then value .&. 0xFF else value `shiftR` 8
        (_, back)     = splitAt 1 rest
relocSymInCode _ _ code = code


getAllSyms :: [PlacedSec] -> SymTable
getAllSyms []                                    = []
getAllSyms (PlacedSec (Sec _ _ syms _) _ : secs) = syms ++ getAllSyms secs

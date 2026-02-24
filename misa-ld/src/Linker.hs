module Linker (linkBinaryObjects) where


import MemoryMap
import Placer
import Relocator

import Data.Word (Word8)
import Grammar
import ObjectFile


type BinaryExe = [Word8]


data LinkError
  = DuplicateSyms [Label]
  | MissingSyms [Label]
  | OrphanedSecs [Label]
  | CodeTooLarge [Label]
  deriving Show



getDuplicateSyms :: [BinaryObject] -> [Label]
getDuplicateSyms _ = []


getMissingSyms :: [BinaryObject]-> [Label]
getMissingSyms _ = []


getOrphanedSecs :: [BinaryObject] -> [Label]
getOrphanedSecs _ = []


getOverflowedSecs :: [PlacedSec] -> MemMap -> [Label]
getOverflowedSecs _ _ = []


joinLinkedSecs :: [PlacedSec] -> [Word8] -> [Word8]
joinLinkedSecs (PlacedSec (Sec _ (LiteralCode arr : []) _ _) start : secs) acc
  = padding ++ arr ++ (joinLinkedSecs secs acc)
  where padding = replicate (fromIntegral start - length acc) (0x00 :: Word8)
joinLinkedSecs _ acc = acc


linkBinaryObjectsSafe :: [BinaryObject] -> MemMap -> Either LinkError BinaryExe
linkBinaryObjectsSafe objs memmap
  | overflowedSecs /= [] = Left (CodeTooLarge overflowedSecs)
  | otherwise            = Right (joinLinkedSecs relocdSecs [])
  where placedSecs     = placeSecs objs memmap
        overflowedSecs = getOverflowedSecs placedSecs memmap
        allSyms        = getAllSyms placedSecs
        relocdSecs     = applyRelocs placedSecs allSyms


linkBinaryObjects :: [BinaryObject] -> MemMap -> Either LinkError BinaryExe
linkBinaryObjects objs memmap
  | duplicateSyms /= [] = Left (DuplicateSyms duplicateSyms)
  | missingSyms /= []   = Left (MissingSyms missingSyms)
  | orphanedSecs /= []  = Left (OrphanedSecs orphanedSecs)
  | otherwise           = linkBinaryObjectsSafe objs memmap
  where duplicateSyms = getDuplicateSyms objs
        missingSyms   = getMissingSyms objs
        orphanedSecs  = getOrphanedSecs objs

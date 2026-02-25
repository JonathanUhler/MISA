module Linker (linkBinaryObjects) where


import MemoryMap
import Placer
import Relocator

import Data.List (sort, group)
import qualified Data.Set as Set
import Debug.Trace (traceM)
import Data.Word (Word8)
import Grammar
import ObjectFile


type BinaryExe = [Word8]


data LinkError
  = DuplicateSyms [Label]
  | MissingSyms [Label]
  | OrphanedSecs [Label]
  | MultiMappedSecs [Label]
  | CodeTooLarge [Label]
  deriving Show


getAllSymDefs :: [BinaryObject] -> [Label]
getAllSymDefs objs = concatMap (concatMap namesFromSec) objs
  where namesFromSec = \(Sec _ _ syms _) -> map nameFromSym syms
        nameFromSym  = \(Sym name _) -> name


getAllRelocDefs :: [BinaryObject] -> [Label]
getAllRelocDefs objs = concatMap (concatMap namesFromSec) objs
  where namesFromSec  = \(Sec _ _ _ relocs) -> map nameFromReloc relocs
        nameFromReloc = \(Reloc _ _ name) -> name


getAllSecDefs :: [BinaryObject] -> [Label]
getAllSecDefs objs = map (concatMap (\(Sec name _ _ _) -> name)) objs


getAllMappedSecs :: MemMap -> [Label]
getAllMappedSecs regions = concatMap (\(MemRegion _ _ secs) -> secs) regions


getDuplicateSyms :: [BinaryObject] -> [Label]
getDuplicateSyms objs = [x | (x : xs) <- group (sort (getAllSymDefs objs)), length (x : xs) > 1]


getMissingSyms :: [BinaryObject]-> [Label]
getMissingSyms objs = Set.toList (Set.difference relocs syms)
  where syms   = Set.fromList (getAllSymDefs objs)
        relocs = Set.fromList (getAllRelocDefs objs)


getOrphanedSecs :: [BinaryObject] -> MemMap -> [Label]
getOrphanedSecs objs memmap = Set.toList (Set.difference secDefs mappedSecs)
  where secDefs    = Set.fromList (getAllSecDefs objs)
        mappedSecs = Set.fromList (getAllMappedSecs memmap)


getMultiMappedSecs :: MemMap -> [Label]
getMultiMappedSecs memmap =
  [x | (x : xs) <- group (sort (getAllMappedSecs memmap)), length (x : xs) > 1]


getOverflowedSecs :: [PlacedSec] -> MemMap -> [Label]
getOverflowedSecs _ [] = []
getOverflowedSecs secs (MemRegion start end names : regions)
  | totalSecSize <= (end - start) =          getOverflowedSecs secs regions
  | otherwise                     = names ++ getOverflowedSecs secs regions
  where secsInRegion = filter (\(PlacedSec (Sec name _ _ _) _) -> elem name names) secs
        totalSecSize = foldr (\(PlacedSec sec _) size -> size + getSecSize sec) 0 secsInRegion


joinLinkedSecs :: [PlacedSec] -> [Word8] -> [Word8]
joinLinkedSecs (PlacedSec (Sec _ (LiteralCode arr : []) _ _) start : secs) acc
  = joinLinkedSecs secs (acc ++ padding ++ arr)
  where padding = replicate (max (fromIntegral start - length acc) 0) (0x00 :: Word8)
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
  | duplicateSyms /= []   = Left (DuplicateSyms duplicateSyms)
  | missingSyms /= []     = Left (MissingSyms missingSyms)
  | orphanedSecs /= []    = Left (OrphanedSecs orphanedSecs)
  | multiMappedSecs /= [] = Left (MultiMappedSecs multiMappedSecs)
  | otherwise             = linkBinaryObjectsSafe objs memmap
  where duplicateSyms   = getDuplicateSyms objs
        missingSyms     = getMissingSyms objs
        orphanedSecs    = getOrphanedSecs objs memmap
        multiMappedSecs = getMultiMappedSecs memmap

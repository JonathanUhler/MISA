module Placer (PlacedSec(..), placeSecs, getPlacedSyms, getPlacedRelocs) where


import MemoryMap
import Data.Word (Word16)
import ObjectFile
import Grammar


data PlacedSec = PlacedSec Sec Word16
  deriving (Show, Eq)


placeSecs :: [BinaryObject] -> MemMap -> [PlacedSec]
placeSecs objs memmap = concatMap (placeObjsInRegion objs) memmap


getPlacedSyms :: [PlacedSec] -> SymTable
getPlacedSyms placedSecs = concatMap (\(PlacedSec (Sec _ _ syms _) _) -> syms) placedSecs


getPlacedRelocs :: [PlacedSec] -> RelocTable
getPlacedRelocs placedSecs = concatMap getAbsoluteRelocs placedSecs
  where getAbsoluteRelocs (PlacedSec (Sec _ _ _ relocs) place) = map (getAbsoluteReloc place) relocs
        getAbsoluteReloc place (Reloc kind addr label)         = Reloc kind (addr + place) label


placeObjsInRegion :: [BinaryObject] -> MemRegion -> [PlacedSec]
placeObjsInRegion objs (MemRegion start _ names) = concatMap placeObjsByName names
  where placeObjsByName name = placeSecsInRegion (extractSecs objs name) start


extractSecs :: [BinaryObject] -> Label -> [Sec]
extractSecs objs name = concatMap extractSecsInObj objs
  where extractSecsInObj = filter (\(Sec sec _ _ _) -> sec == name)


placeSecsInRegion :: [Sec] -> Word16 -> [PlacedSec]
placeSecsInRegion [] _               = []
placeSecsInRegion (sec : secs) start = placedSec : placeSecsInRegion secs (start + getSecSize sec)
  where placedSec = placeSecAt sec start


placeSecAt :: Sec -> Word16 -> PlacedSec
placeSecAt (Sec name code syms relocs) place = PlacedSec (Sec name code newSyms relocs) place
  where newSyms = map (\(Sym label addr) -> Sym label (addr + place)) syms

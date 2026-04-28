module RelocatorSpec (spec) where


import Relocator
import Placer
import ObjectFile
import Grammar

import Test.Hspec


spec :: Spec
spec = do
  describe "applyRelocs" $ do
    it "returns empty list for empty placed sections" $
      applyRelocs [] [] `shouldBe` []
    it "preserves sections without relocations" $ do
      let code = [InstCode (AddInst R0 RA RB)]
      let sec = Sec "text" code [] []
      let placed = PlacedSec sec 0
      let result = applyRelocs [placed] []
      result `shouldBe` [placed]
    it "applies relocations to single placed section" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "symbol"
      let sec = Sec "text" code [] [reloc]
      let placed = PlacedSec sec 0
      let sym = Sym "symbol" 0x42
      let result = applyRelocs [placed] [sym]
      result `shouldBe` [PlacedSec (Sec "text" [LiteralCode [0x42]] [] []) 0]
    it "preserves multiple placed sections in order" $ do
      let code1 = [InstCode (AddInst R0 R0 R0)]
      let code2 = [InstCode (SubInst R0 R0 R0)]
      let sec1 = Sec "text" code1 [] []
      let sec2 = Sec "data" code2 [] []
      let placed1 = PlacedSec sec1 0
      let placed2 = PlacedSec sec2 2
      let result = applyRelocs [placed1, placed2] []
      result `shouldBe` [placed1, placed2]

  describe "relocSec" $ do
    it "returns section with empty relocation table after processing" $ do
      let code = [InstCode (HaltInst R0)]
      let sec = Sec "text" code [] []
      let placed = PlacedSec sec 0
      let result = relocSec placed []
      case result of
        PlacedSec (Sec _ _ _ relocs) _ -> relocs `shouldBe` []
    it "clears all relocations from section" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "test"
      let sec = Sec "text" code [] [reloc]
      let placed = PlacedSec sec 0
      let result = relocSec placed []
      case result of
        PlacedSec (Sec _ _ _ relocs) _ -> relocs `shouldBe` []
    it "preserves section name after relocation" $ do
      let sec = Sec "mysec" [] [] []
      let placed = PlacedSec sec 0
      let result = relocSec placed []
      case result of
        PlacedSec (Sec name _ _ _) _ -> name `shouldBe` "mysec"
    it "preserves section placement address" $ do
      let sec = Sec "text" [] [] []
      let placed = PlacedSec sec 100
      let result = relocSec placed []
      case result of
        PlacedSec _ addr -> addr `shouldBe` 100

  describe "relocSymInCode" $ do
    it "resolves single relocation with matching symbol" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "sym"
      let sym = Sym "sym" 0x42
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0x42
        _                   -> fail "no code returned"
    it "applies low byte mask for LowReloc" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "sym"
      let sym = Sym "sym" 0x1234
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0x34
        _                   -> fail "no code returned"
    it "applies high byte mask for HighReloc" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc HighReloc 0 "sym"
      let sym = Sym "sym" 0x1234
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0x12
        _                   -> fail "no code returned"
    it "resolves relocation at correct offset in code" $ do
      let code = [LiteralCode [0xFF, 0xFF, 0xFF]]
      let reloc = Reloc LowReloc 2 "sym"
      let sym = Sym "sym" 0x99
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> do
          bytes !! 0 `shouldBe` 0xFF
          bytes !! 1 `shouldBe` 0xFF
          bytes !! 2 `shouldBe` 0x99
        _                   -> fail "no code returned"
    it "searches multiple symbols for match" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "sym2"
      let syms = [Sym "sym1" 0x11, Sym "sym2" 0x22]
      let result = relocSymInCode syms reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0x22
        _                   -> fail "no code returned"
    it "returns code unchanged when no symbol matches" $ do
      let code = [LiteralCode [0xFF]]
      let reloc = Reloc LowReloc 0 "nomatch"
      let sym = Sym "sym" 0x42
      let result = relocSymInCode [sym] reloc code
      result `shouldBe` code
    it "returns code unchanged for non-literal code" $ do
      let code = [InstCode (AddInst R0 R0 R0)]
      let reloc = Reloc LowReloc 0 "sym"
      let sym = Sym "sym" 0x42
      let result = relocSymInCode [sym] reloc code
      result `shouldBe` code
    it "preserves other bytes when relocating at offset" $ do
      let code = [LiteralCode [0xAA, 0xBB, 0xCC, 0xDD]]
      let reloc = Reloc LowReloc 1 "sym"
      let sym = Sym "sym" 0xEE
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> do
          bytes !! 0 `shouldBe` 0xAA
          bytes !! 1 `shouldBe` 0xEE
          bytes !! 2 `shouldBe` 0xCC
          bytes !! 3 `shouldBe` 0xDD
        _                   -> fail "no code returned"
    it "handles zero value symbol" $ do
      let code = [LiteralCode [0xFF]]
      let reloc = Reloc LowReloc 0 "sym"
      let sym = Sym "sym" 0
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0x00
        _                   -> fail "no code returned"
    it "handles maximum value symbol" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "sym"
      let sym = Sym "sym" 0xFFFF
      let result = relocSymInCode [sym] reloc code
      case result of
        [LiteralCode bytes] -> bytes !! 0 `shouldBe` 0xFF
        _                   -> fail "no code returned"

  describe "getAllSyms" $ do
    it "returns empty list for empty placed sections" $
      getAllSyms [] `shouldBe` []
    it "extracts single symbol from single section" $ do
      let sym = Sym "start" 0
      let sec = Sec "text" [] [sym] []
      let placed = PlacedSec sec 0
      getAllSyms [placed] `shouldBe` [sym]
    it "extracts multiple symbols from single section" $ do
      let syms = [Sym "start" 0, Sym "end" 10]
      let sec = Sec "text" [] syms []
      let placed = PlacedSec sec 0
      getAllSyms [placed] `shouldBe` syms
    it "concatenates symbols from multiple sections in order" $ do
      let syms1 = [Sym "sym1" 0]
      let syms2 = [Sym "sym2" 5]
      let sec1 = Sec "text" [] syms1 []
      let sec2 = Sec "data" [] syms2 []
      let placed1 = PlacedSec sec1 0
      let placed2 = PlacedSec sec2 10
      getAllSyms [placed1, placed2] `shouldBe` [Sym "sym1" 0, Sym "sym2" 5]
    it "preserves symbol addresses from all sections" $ do
      let sym1 = Sym "first" 42
      let sym2 = Sym "second" 100
      let sec1 = Sec "text" [] [sym1] []
      let sec2 = Sec "data" [] [sym2] []
      let placed1 = PlacedSec sec1 0
      let placed2 = PlacedSec sec2 200
      let result = getAllSyms [placed1, placed2]
      result `shouldBe` [sym1, sym2]
    it "handles sections without symbols" $ do
      let sec1 = Sec "text" [] [] []
      let sec2 = Sec "data" [] [] []
      let placed1 = PlacedSec sec1 0
      let placed2 = PlacedSec sec2 10
      getAllSyms [placed1, placed2] `shouldBe` []

  describe "Edge cases" $ do
    it "handles section with empty code and no relocations" $ do
      let sec = Sec "empty" [] [] []
      let placed = PlacedSec sec 0
      let result = applyRelocs [placed] []
      length result `shouldBe` 1
    it "handles relocation at end of code" $ do
      let code = [LiteralCode [0xAA, 0xBB, 0xCC, 0xDD]]
      let reloc = Reloc LowReloc 3 "sym"
      let sec = Sec "text" code [] [reloc]
      let placed = PlacedSec sec 0
      let syms = [Sym "sym" 0xEE]
      let result = applyRelocs [placed] syms
      case result of
        [PlacedSec (Sec _ [LiteralCode bytes] _ _) _] -> bytes !! 3 `shouldBe` 0xEE
        _                                             -> fail "no sections returned"
    it "handles symbol with zero address" $ do
      let code = [LiteralCode [0xFF]]
      let reloc = Reloc LowReloc 0 "zero_sym"
      let sec = Sec "text" code [] [reloc]
      let placed = PlacedSec sec 0
      let syms = [Sym "zero_sym" 0]
      let result = applyRelocs [placed] syms
      case result of
        [PlacedSec (Sec _ [LiteralCode bytes] _ _) _] -> bytes !! 0 `shouldBe` 0x00
        _                                             -> fail "no sections returned"
    it "handles symbol with maximum address" $ do
      let code = [LiteralCode [0x00]]
      let reloc = Reloc LowReloc 0 "max_sym"
      let sec = Sec "text" code [] [reloc]
      let placed = PlacedSec sec 0
      let syms = [Sym "max_sym" 0xFFFF]
      let result = applyRelocs [placed] syms
      case result of
        [PlacedSec (Sec _ [LiteralCode bytes] _ _) _] -> bytes !! 0 `shouldBe` 0xFF
        _                                             -> fail "no sections returned"

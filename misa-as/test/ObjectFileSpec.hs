module ObjectFileSpec (spec) where


import ObjectFile
import Grammar
import Packing

import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Text.Megaparsec
import Test.Hspec


spec :: Spec
spec = do
  describe "packInst" $ do
    it "packs HALT" $
      packInst (HaltInst R0) `shouldBe` [0x00, 0x00]
    it "packs HALT with different register" $
      packInst (HaltInst RA) `shouldBe` [0x10, 0x00]
    it "packs ADD with all same registers" $
      packInst (AddInst R0 R0 R0) `shouldBe` [0x01, 0x00]
    it "packs ADD with different registers" $
      packInst (AddInst RA RB RC) `shouldBe` [0x11, 0x32]
    it "packs ADC" $
      packInst (AdcInst R0 R0 R0) `shouldBe` [0x02, 0x00]
    it "packs ADC with different registers" $
      packInst (AdcInst RC RD RE) `shouldBe` [0x32, 0x54]
    it "packs SUB" $
      packInst (SubInst R0 R0 R0) `shouldBe` [0x03, 0x00]
    it "packs SUB with different registers" $
      packInst (SubInst RA RB RC) `shouldBe` [0x13, 0x32]
    it "packs SBB" $
      packInst (SbbInst R0 R0 R0) `shouldBe` [0x04, 0x00]
    it "packs SBB with different registers" $
      packInst (SbbInst RC RD RE) `shouldBe` [0x34, 0x54]
    it "packs AND" $
      packInst (AndInst R0 R0 R0) `shouldBe` [0x05, 0x00]
    it "packs AND with different registers" $
      packInst (AndInst RA RB RC) `shouldBe` [0x15, 0x32]
    it "packs OR" $
      packInst (OrInst R0 R0 R0) `shouldBe` [0x06, 0x00]
    it "packs OR with different registers" $
      packInst (OrInst RA RB RC) `shouldBe` [0x16, 0x32]
    it "packs XOR" $
      packInst (XorInst R0 R0 R0) `shouldBe` [0x07, 0x00]
    it "packs XOR with different registers" $
      packInst (XorInst RA RB RC) `shouldBe` [0x17, 0x32]
    it "packs RRC" $
      packInst (RrcInst R0 R0) `shouldBe` [0x08, 0x00]
    it "packs RRC with different registers" $
      packInst (RrcInst RA RB) `shouldBe` [0x18, 0x02]
    it "packs LW" $
      packInst (LwInst R0 R0 R0) `shouldBe` [0x0A, 0x00]
    it "packs LW with different registers" $
      packInst (LwInst RA RB RC) `shouldBe` [0x1A, 0x32]
    it "packs SW" $
      packInst (SwInst R0 R0 R0) `shouldBe` [0x0B, 0x00]
    it "packs SW with different registers" $
      packInst (SwInst RA RB RC) `shouldBe` [0x1B, 0x32]
    it "packs SET with IntImm Low" $
      packInst (SetInst R0 (IntImm Low 0x42)) `shouldBe` [0x09, 0x42]
    it "packs SET with IntImm Low different register" $
      packInst (SetInst RA (IntImm Low 0xFF)) `shouldBe` [0x19, 0xFF]
    it "packs SET with IntImm High" $
      packInst (SetInst R0 (IntImm High 0x1234)) `shouldBe` [0x09, 0x12]
    it "packs SET with IntImm Full" $
      packInst (SetInst RB (IntImm Full 0x99)) `shouldBe` [0x29, 0x99]
    it "packs SET with LabelImm Low (placeholder)" $
      packInst (SetInst R0 (LabelImm Low "test")) `shouldBe` [0x09, 0x00]
    it "packs SET with LabelImm High (placeholder)" $
      packInst (SetInst RA (LabelImm High "test")) `shouldBe` [0x19, 0x00]
    it "packs RSR with SADDR" $
      packInst (RsrInst SADDR R0 R0) `shouldBe` [0x0C, 0x10]
    it "packs RSR with RADDR" $
      packInst (RsrInst RADDR RA RB) `shouldBe` [0x1C, 0x22]
    it "packs RSR with FLAGS" $
      packInst (RsrInst FLAGS RC RD) `shouldBe` [0x3C, 0x34]
    it "packs RSR with CAUSE" $
      packInst (RsrInst CAUSE RE RF) `shouldBe` [0x5C, 0x46]
    it "packs WSR with SADDR" $
      packInst (WsrInst SADDR R0 R0) `shouldBe` [0x0D, 0x10]
    it "packs WSR with RADDR" $
      packInst (WsrInst RADDR RA RB) `shouldBe` [0x1D, 0x22]
    it "packs WSR with FLAGS" $
      packInst (WsrInst FLAGS RC RD) `shouldBe` [0x3D, 0x34]
    it "packs WSR with CAUSE" $
      packInst (WsrInst CAUSE RE RF) `shouldBe` [0x5D, 0x46]
    it "packs JAL with ALWAYS" $
      packInst (JalInst ALWAYS R0 R0) `shouldBe` [0x0E, 0x00]
    it "packs JAL with EQUAL" $
      packInst (JalInst EQUAL RA RB) `shouldBe` [0x1E, 0x12]
    it "packs JAL with NOT_EQUAL" $
      packInst (JalInst NOT_EQUAL RC RD) `shouldBe` [0x3E, 0x84]
    it "packs JAL with GREATER" $
      packInst (JalInst GREATER RE RF) `shouldBe` [0x5E, 0x26]
    it "packs JAL with LESS" $
      packInst (JalInst LESS RU RV) `shouldBe` [0x7E, 0x48]
    it "packs JAL with GREATER_EQUAL" $
      packInst (JalInst GREATER_EQUAL RW RX) `shouldBe` [0x9E, 0x3A]
    it "packs JAL with LESS_EQUAL" $
      packInst (JalInst LESS_EQUAL RY RZ) `shouldBe` [0xBE, 0x5C]
    it "packs JAL with ALWAYS" $
      packInst (JmpInst ALWAYS R0 R0) `shouldBe` [0x0F, 0x00]
    it "packs JMP with EQUAL" $
      packInst (JmpInst EQUAL RA RB) `shouldBe` [0x1F, 0x12]
    it "packs JMP with NOT_EQUAL" $
      packInst (JmpInst NOT_EQUAL RC RD) `shouldBe` [0x3F, 0x84]
    it "packs JMP with GREATER" $
      packInst (JmpInst GREATER RE RF) `shouldBe` [0x5F, 0x26]
    it "packs JMP with LESS" $
      packInst (JmpInst LESS RU RV) `shouldBe` [0x7F, 0x48]
    it "packs JMP with GREATER_EQUAL" $
      packInst (JmpInst GREATER_EQUAL RW RX) `shouldBe` [0x9F, 0x3A]
    it "packs JMP with LESS_EQUAL" $
      packInst (JmpInst LESS_EQUAL RY RZ) `shouldBe` [0xBF, 0x5C]

  describe "packCodeElem" $ do
    it "packs single instruction" $
      packCodeElem (InstCode (HaltInst R0)) `shouldBe` [0x00, 0x00]
    it "packs instruction with different register" $
      packCodeElem (InstCode (AddInst RA RB RC)) `shouldBe` [0x11, 0x32]
    it "packs instruction returns exactly 2 bytes" $
      length (packCodeElem (InstCode (SetInst R0 (IntImm Low 42)))) `shouldBe` 2
    it "packs empty literal code" $
      packCodeElem (LiteralCode []) `shouldBe` []
    it "packs single byte literal" $
      packCodeElem (LiteralCode [0x42]) `shouldBe` [0x42]
    it "packs multiple byte literal" $
      packCodeElem (LiteralCode [0x01, 0x02, 0x03]) `shouldBe` [0x01, 0x02, 0x03]
    it "packs literal from .word directive" $
      packCodeElem (LiteralCode [0xFF]) `shouldBe` [0xFF]
    it "packs literal from .array directive" $
      packCodeElem (LiteralCode [0x00, 0x11, 0x22, 0x33]) `shouldBe` [0x00, 0x11, 0x22, 0x33]

  describe "packCode" $ do
    it "packs empty code with length prefix" $
      packCode [] `shouldBe` [0x00, 0x00]
    it "packs single instruction with length prefix" $
      packCode [InstCode (HaltInst R0)] `shouldBe` [0x02, 0x00, 0x00, 0x00]
    it "packs multiple instructions with length prefix" $ do
      let code = [InstCode (HaltInst R0), InstCode (AddInst R0 R0 R0)]
      packCode code `shouldBe` [0x04, 0x00, 0x00, 0x00, 0x01, 0x00]
    it "packs literal code with length prefix" $
      packCode [LiteralCode [0xFF, 0xAA]] `shouldBe` [0x02, 0x00, 0xFF, 0xAA]
    it "packs mixed instructions and literals with length prefix" $ do
      let code = [InstCode (HaltInst R0), LiteralCode [0x42, 0x43]]
      packCode code `shouldBe` [0x04, 0x00, 0x00, 0x00, 0x42, 0x43]
    it "packs code length correctly for large code sections" $ do
      let code = replicate 10 (InstCode (HaltInst R0))
      let packed = packCode code
      take 2 packed `shouldBe` [0x14, 0x00]

  describe "packSym" $ do
    it "packs symbol with simple name" $
      let sym = Sym "test" 100
      in take 2 (packSym sym) `shouldBe` packDoubleWord 100
    it "packs symbol with address 0" $
      let sym = Sym "start" 0
      in take 4 (packSym sym) `shouldBe` [0x00, 0x00, 0x05, 0x00]
    it "packs symbol with maximum address" $
      let sym = Sym "end" 0xFFFF
      in take 4 (packSym sym) `shouldBe` [0xFF, 0xFF, 0x03, 0x00]
    it "packs symbol name after address" $
      let sym = Sym "label" 42
          packed = packSym sym
          nameBytes = drop 2 packed
      in take 2 nameBytes `shouldBe` [0x05, 0x00]

  describe "packSyms" $ do
    it "packs empty symbol table" $
      packSyms [] `shouldBe` [0x00, 0x00]
    it "packs single symbol" $ do
      let sym = Sym "start" 0
          packed = packSyms [sym]
      take 2 packed `shouldBe` [0x01, 0x00]
    it "packs multiple symbols" $ do
      let syms = [Sym "start" 0, Sym "end" 100]
          packed = packSyms syms
      take 2 packed `shouldBe` [0x02, 0x00]
    it "packs symbols in order" $ do
      let syms = [Sym "first" 10, Sym "second" 20]
          packed = packSyms syms
      take 4 packed `shouldBe` [0x02, 0x00, 0x0A, 0x00]

  describe "packReloc" $ do
    it "packs relocation with LowReloc type" $
      let reloc = Reloc LowReloc 42 "symbol"
          packed = packReloc reloc
      in packed !! 0 `shouldBe` fromIntegral (fromEnum LowReloc)
    it "packs relocation with HighReloc type" $
      let reloc = Reloc HighReloc 100 "label"
          packed = packReloc reloc
      in packed !! 0 `shouldBe` fromIntegral (fromEnum HighReloc)
    it "packs relocation address in little-endian" $
      let reloc = Reloc LowReloc 0x1234 "test"
          packed = packReloc reloc
      in take 3 packed `shouldBe` [0x00, 0x34, 0x12]
    it "packs relocation name after type and address" $
      let reloc = Reloc LowReloc 0 "label"
          packed = packReloc reloc
          nameBytes = drop 3 packed
      in take 2 nameBytes `shouldBe` [0x05, 0x00]

  describe "packRelocs" $ do
    it "packs empty relocation table" $
      packRelocs [] `shouldBe` [0x00, 0x00]
    it "packs single relocation" $ do
      let reloc = Reloc LowReloc 0 "symbol"
          packed = packRelocs [reloc]
      take 2 packed `shouldBe` [0x01, 0x00]
    it "packs multiple relocations" $ do
      let relocs = [Reloc LowReloc 0 "s1", Reloc HighReloc 2 "s2"]
          packed = packRelocs relocs
      take 2 packed `shouldBe` [0x02, 0x00]

  describe "packSec" $ do
    it "packs section with empty components" $
      let sec = Sec "text" [] [] []
          packed = packSec sec
      in length packed > 0
    it "packs section name" $ do
      let sec = Sec "code" [] [] []
          packed = packSec sec
      take 2 packed `shouldBe` [0x04, 0x00]
    it "packs section with code" $ do
      let code = [InstCode (HaltInst R0)]
          sec = Sec "text" code [] []
          packed = packSec sec
      length packed > 6
    it "packs section with symbol" $ do
      let code = [InstCode (HaltInst R0)]
          sym = Sym "start" 0
          sec = Sec "text" code [sym] []
          packed = packSec sec
      length packed > 10
    it "packs section with relocation" $ do
      let code = [InstCode (HaltInst R0)]
          reloc = Reloc LowReloc 0 "symbol"
          sec = Sec "text" code [] [reloc]
          packed = packSec sec
      length packed > 10

  describe "packBinaryObject" $ do
    it "packs empty binary object" $ do
      packBinaryObject [] `shouldBe` (B.unpack (encodeUtf8 (pack "MISA_LF ")) ++ [0x00, 0x00])
    it "packs binary object with single section" $ do
      let sec = Sec "text" [] [] []
          packed = packBinaryObject [sec]
      take 8 packed `shouldBe` B.unpack (encodeUtf8 (pack "MISA_LF "))
      take 10 (drop 8 packed) `shouldBe` [0x01, 0x00, 0x04, 0x00, 116, 101, 120, 116, 0x00, 0x00]

  describe "unpackCode" $ do
    it "unpacks empty code" $
      case runParser unpackCode "" (B.pack [0x00, 0x00]) of
        Right code -> code `shouldBe` [LiteralCode []]
        Left _     -> fail "Failed to unpack empty code"
    it "unpacks single byte code" $
      case runParser unpackCode "" (B.pack [0x01, 0x00, 0x42]) of
        Right code -> code `shouldBe` [LiteralCode [0x42]]
        Left _     -> fail "Failed to unpack single byte"
    it "unpacks multiple byte code" $
      case runParser unpackCode "" (B.pack [0x04, 0x00, 0x01, 0x02, 0x03, 0x04]) of
        Right code -> code `shouldBe` [LiteralCode [0x01, 0x02, 0x03, 0x04]]
        Left _     -> fail "Failed to unpack multiple bytes"

  describe "unpackSym" $ do
    it "unpacks symbol with address" $
      let bytes = B.pack (packDoubleWord 42 ++ packString "test")
      in case runParser unpackSym "" bytes of
        Right (Sym name addr) -> do
          name `shouldBe` "test"
          addr `shouldBe` 42
        Left _ -> fail "Failed to unpack symbol"
    it "unpacks symbol with zero address" $
      let bytes = B.pack (packDoubleWord 0 ++ packString "start")
      in case runParser unpackSym "" bytes of
        Right (Sym name addr) -> do
          name `shouldBe` "start"
          addr `shouldBe` 0
        Left _ -> fail "Failed to unpack symbol"

  describe "unpackSyms" $ do
    it "unpacks empty symbol table" $
      case runParser unpackSyms "" (B.pack [0x00, 0x00]) of
        Right syms -> syms `shouldBe` []
        Left _     -> fail "Failed to unpack empty symbol table"
    it "unpacks single symbol" $
      let bytes = B.pack ([0x01, 0x00] ++ packDoubleWord 0 ++ packString "symbol")
      in case runParser unpackSyms "" bytes of
        Right syms -> length syms `shouldBe` 1
        Left _     -> fail "Failed to unpack single symbol"

  describe "unpackReloc" $ do
    it "unpacks relocation with LowReloc type" $
      let bytes =
            B.pack ([fromIntegral (fromEnum LowReloc)] ++ packDoubleWord 50 ++ packString "sym")
      in case runParser unpackReloc "" bytes of
        Right (Reloc kind addr name) -> do
          kind `shouldBe` LowReloc
          addr `shouldBe` 50
          name `shouldBe` "sym"
        Left _ -> fail "Failed to unpack relocation"
    it "unpacks relocation with HighReloc type" $
      let bytes =
            B.pack ([fromIntegral (fromEnum HighReloc)] ++ packDoubleWord 100 ++ packString "label")
      in case runParser unpackReloc "" bytes of
        Right (Reloc kind addr name) -> do
          kind `shouldBe` HighReloc
          addr `shouldBe` 100
          name `shouldBe` "label"
        Left _ -> fail "Failed to unpack relocation"

  describe "unpackRelocs" $ do
    it "unpacks empty relocation table" $
      case runParser unpackRelocs "" (B.pack [0x00, 0x00]) of
        Right relocs -> relocs `shouldBe` []
        Left _       -> fail "Failed to unpack empty relocation table"
    it "unpacks single relocation" $
      let bytes =
            B.pack ([0x01, 0x00, fromIntegral (fromEnum LowReloc)] ++
                    packDoubleWord 0 ++ packString "s")
      in case runParser unpackRelocs "" bytes of
        Right relocs -> length relocs `shouldBe` 1
        Left _       -> fail "Failed to unpack single relocation"

  describe "unpackSec" $ do
    it "unpacks section with all components" $
      let code = packCode []
          syms = packSyms []
          relocs = packRelocs []
          bytes = B.pack (packString "text" ++ code ++ syms ++ relocs)
      in case runParser unpackSec "" bytes of
        Right (Sec name _ _ _) -> name `shouldBe` "text"
        Left _                 -> fail "Failed to unpack section"

  describe "unpackBinaryObject" $ do
    it "unpacks empty binary object" $
      let bytes = B.pack (B.unpack (encodeUtf8 (pack "MISA_LF ")) ++ [0x00, 0x00])
      in case runParser unpackBinaryObject "" bytes of
        Right obj -> obj `shouldBe` []
        Left _    -> fail "Failed to unpack empty binary object"
    it "unpacks binary object with sections" $
      let header = B.unpack (encodeUtf8 (pack "MISA_LF "))
          bytes = B.pack (header ++ [0x01, 0x00] ++ packString "text" ++
                          packCode [] ++ packSyms [] ++ packRelocs [])
      in case runParser unpackBinaryObject "" bytes of
        Right obj -> length obj `shouldBe` 1
        Left _    -> fail "Failed to unpack binary object with sections"

  describe "unpackNamedBinaryObject" $ do
    it "unpacks named binary object with empty object" $
      let bytes =
            B.pack (packString "myObj" ++ B.unpack (encodeUtf8 (pack "MISA_LF ")) ++ [0x00, 0x00])
      in case runParser unpackNamedBinaryObject "" bytes of
        Right (NamedBinaryObject name obj) -> do
          name `shouldBe` "myObj"
          obj `shouldBe` []
        Left _ -> fail "Failed to unpack named binary object"

  describe "Roundtrip" $ do
    it "roundtrips empty binary object" $
      let obj = []
          packed = packBinaryObject obj
          bytes = B.pack packed
      in case runParser unpackBinaryObject "" bytes of
        Right unpacked -> unpacked `shouldBe` obj
        Left _         -> fail "Roundtrip failed"
    it "roundtrips binary object with section" $
      let code = [LiteralCode [0xFF, 0xAA]]
          sec = Sec "text" code [] []
          obj = [sec]
          packed = packBinaryObject obj
          bytes = B.pack packed
      in case runParser unpackBinaryObject "" bytes of
        Right unpacked -> length unpacked `shouldBe` 1
        Left _         -> fail "Roundtrip failed"
    it "roundtrips named binary object with empty object" $
      let nbo = NamedBinaryObject "test" []
          packed = packNamedBinaryObject nbo
          bytes = B.pack packed
      in case runParser unpackNamedBinaryObject "" bytes of
        Right (NamedBinaryObject name obj) -> do
          name `shouldBe` "test"
          obj `shouldBe` []
        Left _ -> fail "Roundtrip failed"

  describe "getSecSize" $ do
    it "returns zero for empty section" $
      getSecSize (Sec "empty" [] [] []) `shouldBe` 0
    it "returns 2 for single instruction" $
      getSecSize (Sec "code" [InstCode (HaltInst R0)] [] []) `shouldBe` 2
    it "returns correct size for multiple instructions" $
      getSecSize (Sec "code" [InstCode (HaltInst R0), InstCode (AddInst R0 R0 R0)] [] []) `shouldBe` 4
    it "returns correct size for literal code" $
      getSecSize (Sec "data" [LiteralCode [0x01, 0x02, 0x03]] [] []) `shouldBe` 3
    it "returns correct size for mixed code" $
      getSecSize (Sec "mixed" [InstCode (HaltInst R0), LiteralCode [0x42, 0x43]] [] []) `shouldBe` 4
    it "accumulates sizes correctly" $ do
      let code = [LiteralCode [0x01], InstCode (HaltInst R0), LiteralCode [0x02, 0x03]]
      getSecSize (Sec "accum" code [] []) `shouldBe` 5

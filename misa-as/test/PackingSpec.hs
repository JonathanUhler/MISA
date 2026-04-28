module PackingSpec (spec) where


import Packing

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Test.Hspec


spec :: Spec
spec = do
  describe "packDoubleWord" $ do
    it "packs zero" $
      packDoubleWord 0 `shouldBe` [0x00, 0x00]
    it "packs maximum Word8" $
      packDoubleWord 0xFF `shouldBe` [0xFF, 0x00]
    it "packs maximum Word16" $
      packDoubleWord 0xFFFF `shouldBe` [0xFF, 0xFF]
    it "packs to little-endian 1" $
      packDoubleWord 0x1234 `shouldBe` [0x34, 0x12]
    it "packs to little-endian 2" $
      packDoubleWord 0x0100 `shouldBe` [0x00, 0x01]
    it "packs to little-endian 3" $
      packDoubleWord 0x0001 `shouldBe` [0x01, 0x00]
    it "always produces exactly 2 bytes" $ do
      length (packDoubleWord 0) `shouldBe` 2
      length (packDoubleWord 0xFFFF) `shouldBe` 2
      length (packDoubleWord 0x5678) `shouldBe` 2

  describe "packString" $ do
    it "packs the empty string" $
      packString "" `shouldBe` [0x00, 0x00]
    it "packs a single character" $ do
      let result = packString "A"
      take 2 result `shouldBe` [0x01, 0x00]
      drop 2 result `shouldBe` B.unpack (E.encodeUtf8 (T.pack "A"))
    it "packs multiple characters" $ do
      let result = packString "hello"
      let len = take 2 result
      let str = drop 2 result
      len `shouldBe` [0x05, 0x00]
      str `shouldBe` B.unpack (E.encodeUtf8 (T.pack "hello"))
    it "starts with length prefix in little-endian" $ do
      let result = packString "test"
      take 2 result `shouldBe` [0x04, 0x00]
    it "handles ASCII characters" $ do
      let result = packString "ABC123"
      let len = take 2 result
      let str = drop 2 result
      len `shouldBe` [0x06, 0x00]
      str `shouldBe` B.unpack (E.encodeUtf8 (T.pack "ABC123"))
    it "handles special ASCII characters" $ do
      let result = packString "!@#$%^&*()"
      let len = take 2 result
      let str = drop 2 result
      len `shouldBe` [0x0A, 0x00]
      str `shouldBe` B.unpack (E.encodeUtf8 (T.pack "!@#$%^&*()"))
    it "length prefix represents byte count" $ do
      let result = packString "hello"
      let prefix = take 2 result
      let dataLen = length (drop 2 result)
      prefix  `shouldBe` [0x05, 0x00]
      dataLen `shouldBe` 5

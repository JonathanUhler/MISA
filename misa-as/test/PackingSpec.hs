module PackingSpec (spec) where



import Packing

import Test.Hspec


spec :: Spec
spec = do
  describe "packString" $ do
    it "packs the empty string" $
      packString "" `shouldBe` [0x00, 0x00]

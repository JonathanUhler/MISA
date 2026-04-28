module ParserSpec (spec) where


import Test.Hspec


spec :: Spec
spec = do
  describe "foo" $ do it "foo" $ 0 `shouldBe` 0

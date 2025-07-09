module Dec21Spec (spec) where

import Dec21
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 21" $ do
  describeInput "example" "21-example.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "126384"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "0"

  describeInput "real input" "21.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "123096"

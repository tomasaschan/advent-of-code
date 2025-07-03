module Dec19Spec (spec) where

import Dec19
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 19" $ do
  describeInput "example" "19-example.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "6"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "16"

  describeInput "real input" "19.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "308"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "662726441391898"

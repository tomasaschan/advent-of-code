module Dec16Spec (spec) where

import Dec16
import Test.Hspec
import Test.Helpers

spec :: Spec
spec = describe "Dec 16" $ do
  describeInput "small example" "16-small.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "7036"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "45"

  describeInput "large example" "16-large.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "11048"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "64"

  describeInput "real input" "16.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "127520"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "565"

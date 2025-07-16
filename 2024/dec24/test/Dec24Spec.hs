module Dec24Spec (spec) where

import Dec24
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 24" $ do
  describeInput "small example" "24-small.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "4"

  -- it "should solve b" $ \(_, b) -> do
  --   b `shouldBe` "0"

  describeInput "larger example" "24-large.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "2024"

  --   it "should solve b" $ \(_, b) -> do
  --     b `shouldBe` "0"

  describeInput "real input" "24.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "36035961805936"

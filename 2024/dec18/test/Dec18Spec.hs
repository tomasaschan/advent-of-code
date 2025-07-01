module Dec18Spec (spec) where

import Dec18
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 18" $ do
  describeInput "example" "18-example.txt" (solve (6, 6) 12) $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "22"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "6,1"

  describeInput "real input" "18.txt" (solve (70, 70) 1024) $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "294"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "31,22"

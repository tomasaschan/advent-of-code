module Dec20Spec (spec) where

import Dec20
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 20" $ do
  describeInput "example" "20-example.txt" (solve 50) $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "1"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "285"

  describeInput "real input" "20.txt" (solve 100) $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "1395"

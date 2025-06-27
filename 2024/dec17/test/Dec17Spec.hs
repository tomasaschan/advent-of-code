module Dec17Spec (spec) where

import Dec17
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 17" $ do
  describeInput "example" "17-example.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "4,6,3,5,6,3,5,2,1,0"

  --   it "should solve b" $ \(_, b) -> do
  --     b `shouldBe` "0"

  describeInput "real input" "17.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "1,3,5,1,7,2,5,1,6"

  --   it "should solve b" $ \(_, b) -> do
  --     b `shouldBe` "0"
  return ()

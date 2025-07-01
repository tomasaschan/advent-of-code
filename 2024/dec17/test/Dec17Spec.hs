module Dec17Spec (spec) where

import Dec17
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 17" $ do
  describeInput "example a" "17-example-a.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "4,6,3,5,6,3,5,2,1,0"

  describeInput "example b" "17-example-b.txt" solve $ do
    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "117440"

  describeInput "real input" "17.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "1,3,5,1,7,2,5,1,6"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "236555997372013"

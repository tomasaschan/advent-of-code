module Dec25Spec (spec) where

import Dec25
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 25" $ do
  describeInput "example" "25-example.txt" solve $ do
    it "should solve" $ \a -> do
      a `shouldBe` "3"

  describeInput "real input" "25.txt" solve $ do
    it "should solve" $ \a -> do
      a `shouldBe` "3338"

module Dec23Spec (spec) where

import Dec23
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 23" $ do
  describeInput "example" "23-example.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "7"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "co,de,ka,ta"

  describeInput "real input" "23.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "1304"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "ao,es,fe,if,in,io,ky,qq,rd,rn,rv,vc,vl"

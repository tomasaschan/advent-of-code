module Dec15Spec (spec) where

import Dec15
import Test.Hspec
import Test.Helpers

spec :: Spec
spec = describe "Dec 15" $ do
  describeInput "small example" "15-smaller.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "2028"

  describeInput "larger example" "15-larger.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "10092"

    it "should solve b" $ \solution -> do
      snd solution `shouldBe` "9021"

  describeInput "real input" "15.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "1514333"

    it "should solve b" $ \solution -> do
      snd solution `shouldBe` "1528453"

module Dec15Spec (spec) where

import Dec15
import Test.Hspec

withInput :: String -> SpecWith (String, String) -> Spec
withInput file = before $ do
  input <- readFile file
  return $ solve input

spec :: Spec
spec = describe "Dec 15" $ do
  describe "small example" $ withInput "../../inputs/2024/15-smaller.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "2028"

  describe "larger example" $ withInput "../../inputs/2024/15-larger.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "10092"

    it "should solve b" $ \solution -> do
      snd solution `shouldBe` "9021"

  describe "real input" $ withInput "../../inputs/2024/15.txt" $ do
    it "should solve a" $ \solution -> do
      fst solution `shouldBe` "1514333"

    it "should solve b" $ \solution -> do
      snd solution `shouldBe` "1528453"

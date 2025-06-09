module Dec15Spec (spec) where

import Dec15
import Test.Hspec

spec :: Spec
spec = describe "Dec 15" $ do
  describe "small example" $ do
    it "should solve a" $ do
      input <- readFile "../../inputs/2024/15-smaller.txt"
      let solution = solve input
      fst solution `shouldBe` "2028"

  describe "larger example" $ do
    it "should solve a" $ do
      input <- readFile "../../inputs/2024/15-larger.txt"
      let solution = solve input
      fst solution `shouldBe` "10092"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/15.txt"
    let solution = solve realInput
    fst solution `shouldBe` "1514333"

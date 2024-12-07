module Dec03Spec (spec) where

import Dec03
import Test.Hspec

spec :: Spec
spec = describe "Dec 3" $ do
  it "should solve a" $ do
    let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    fst (solve input) `shouldBe` "161"

  it "should solve b" $ do
    let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    snd (solve input) `shouldBe` "48"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/03.txt"
    solve realInput `shouldBe` ("163931492", "76911921")

  describe "parse" $ do
    it "should parse do()" $ do
      parse False False "do()mul(2,3)" `shouldBe` [Mul 2 3]

    it "should parse don't()" $ do
      parse False True "don't()mul(2,3)" `shouldBe` []

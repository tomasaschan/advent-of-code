module Dec07Spec (spec) where

import Dec07
import Test.Hspec

spec :: Spec
spec = describe "Dec 7" $ do
  let input =
        "190: 10 19\n\
        \3267: 81 40 27\n\
        \83: 17 5\n\
        \156: 15 6\n\
        \7290: 6 8 6 15\n\
        \161011: 16 10 13\n\
        \192: 17 8 14\n\
        \21037: 9 7 18 13\n\
        \292: 11 6 16 20\n"
  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "3749"

  it "should solve b" $ do
    snd solution `shouldBe` "11387"

  describe "concatenation operator" $ do
    it "should work" $ do
      2 <||> 3 `shouldBe` 23
      2 <||> 30 `shouldBe` 230
      2 <||> 300 `shouldBe` 2300

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/07.txt"
    let (a, b) = solve realInput

    (a, b) `shouldBe` ("1260333054159", "162042343638683")

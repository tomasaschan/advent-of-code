module Dec02Spec (spec) where

import Dec02
import Test.Hspec

spec :: Spec
spec = describe "Dec 2" $ do
  let input =
        "7 6 4 2 1\n\
        \1 2 7 8 9\n\
        \9 7 6 2 1\n\
        \1 3 2 4 5\n\
        \8 6 4 4 1\n\
        \1 3 6 7 9\n\
        \"
  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "2"

  it "should solve b" $ do
    snd solution `shouldBe` "4"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/02.txt"
    solve realInput `shouldBe` ("411", "465")

  describe "safe" $ do
    it "is true when all decrease by 1 or 2" $ do
      safe [7, 6, 4, 2, 1] `shouldBe` True
    it "is false when an increase is too big" $ do
      safe [1, 2, 7, 8, 9] `shouldBe` False
    it "is false when a decrease is too big" $ do
      safe [9, 7, 6, 2, 1] `shouldBe` False
    it "is false when not monotonous" $ do
      safe [1, 3, 2, 4, 5] `shouldBe` False
    it "is false when subsequent numbers are equal" $ do
      safe [8, 6, 4, 4, 1] `shouldBe` False
    it "is true for the last example" $ do
      safe [1, 3, 6, 7, 9] `shouldBe` True

  describe "dampen" $ do
    it "takes no action when none is needed" $ do
      dampen [7, 6, 4, 2, 1] `shouldBe` [7, 6, 4, 2, 1]
      dampen [1, 3, 6, 7, 9] `shouldBe` [1, 3, 6, 7, 9]

    it "takes no action when none helps" $ do
      dampen [1, 2, 7, 8, 9] `shouldBe` [1, 2, 7, 8, 9]
      dampen [9, 7, 6, 2, 1] `shouldBe` [9, 7, 6, 2, 1]

    it "removes single non-monotonous report" $ do
      dampen [1, 3, 2, 4, 5] `shouldBe` [1, 2, 4, 5]

    it "removes duplicate level" $ do
      dampen [8, 6, 4, 4, 1] `shouldBe` [8, 6, 4, 1]

  describe "safe . dampen" $ do
    it "is true for already safe reports" $ do
      (safe . dampen) [7, 6, 4, 2, 1] `shouldBe` True
      (safe . dampen) [1, 3, 6, 7, 9] `shouldBe` True
    it "is false when dampening doesn't help" $ do
      (safe . dampen) [1, 2, 7, 8, 9] `shouldBe` False
      (safe . dampen) [9, 7, 6, 2, 1] `shouldBe` False
    it "can dampen [1, 3, 2, 4, 5]" $ do
      (safe . dampen) [1, 3, 2, 4, 5] `shouldBe` True
    it "can dampen [8, 6, 4, 4, 1]" $ do
      (safe . dampen) [8, 6, 4, 4, 1] `shouldBe` True

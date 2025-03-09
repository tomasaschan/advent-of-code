module Dec12Spec (spec) where

import Dec12
import Test.Hspec

spec :: Spec
spec = describe "Dec 12" $ do
  describe "examples" $ do
    describe "first" $ do
      let input =
            "AAAA\n\
            \BBCD\n\
            \BBCC\n\
            \EEEC\n"

      it "should solve a" $ do
        fst (solve input) `shouldBe` "140"

    describe "second" $ do
      let input =
            "OOOOO\n\
            \OXOXO\n\
            \OOOOO\n\
            \OXOXO\n\
            \OOOOO\n"

      it "should solve a" $ do
        fst (solve input) `shouldBe` "772"

    describe "third" $ do
      let input =
            "RRRRIICCFF\n\
            \RRRRIICCCF\n\
            \VVRRRCCFFF\n\
            \VVRCCCJFFF\n\
            \VVVVCJJCFE\n\
            \VVIVCCJJEE\n\
            \VVIIICJJEE\n\
            \MIIIIIJJEE\n\
            \MIIISIJEEE\n\
            \MMMISSJEEE\n"

      it "should solve a" $ do
        fst (solve input) `shouldBe` "1930"

  --   it "should solve b" $ do
  --     snd solution `shouldBe` "0"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/12.txt"
    solve realInput `shouldBe` ("1359028", "0")

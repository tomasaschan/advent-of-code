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

      it "should solve b" $ do
        snd (solve input) `shouldBe` "80"

      describe "calculating sides" $ do
        let plots = findPlots' $ parse' input
        let plot c = head . fmap fst . filter ((== c) . snd) $ plots

        it "plot A" $ do
          sides (plot 'A') `shouldBe` 4

        it "plot B" $ do
          sides (plot 'B') `shouldBe` 4

        it "plot C" $ do
          sides (plot 'C') `shouldBe` 8

        it "plot D" $ do
          sides (plot 'D') `shouldBe` 4

        it "plot E" $ do
          sides (plot 'E') `shouldBe` 4

    describe "second" $ do
      let input =
            "OOOOO\n\
            \OXOXO\n\
            \OOOOO\n\
            \OXOXO\n\
            \OOOOO\n"

      it "should solve a" $ do
        fst (solve input) `shouldBe` "772"

      it "should solve b" $ do
        snd (solve input) `shouldBe` "436"

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

    describe "fourth" $ do
      let input =
            "EEEEE\n\
            \EXXXX\n\
            \EEEEE\n\
            \EXXXX\n\
            \EEEEE\n"

      it "should solve b" $ do
        snd (solve input) `shouldBe` "236"

    describe "fifth" $ do
      let input =
            "AAAAAA\n\
            \AAABBA\n\
            \AAABBA\n\
            \ABBAAA\n\
            \ABBAAA\n\
            \AAAAAA\n"

      it "should solve b" $ do
        snd (solve input) `shouldBe` "368"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/12.txt"
    solve realInput `shouldBe` ("1359028", "839780")

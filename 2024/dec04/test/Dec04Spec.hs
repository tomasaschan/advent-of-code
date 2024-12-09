module Dec04Spec (spec) where

import Dec04
import Test.Hspec

spec :: Spec
spec = describe "Dec 4" $ do
  let input =
        "MMMSXXMASM\n\
        \MSAMXMSMSA\n\
        \AMXSXMAAMM\n\
        \MSAMASMSMX\n\
        \XMASAMXAMM\n\
        \XXAMMXXAMA\n\
        \SMSMSASXSS\n\
        \SAXAMASAAA\n\
        \MAMMMXMMMM\n\
        \MXMXAXMASX\n"
  let solution = solve input

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/04.txt"
    solve realInput `shouldBe` ("2549", "2003")

  it "should solve a" $ do
    fst solution `shouldBe` "18"

  it "should parse and show a grid correctly" $ do
    let grid = parse input
    show grid `shouldBe` input

  it "should solve b" $ do
    snd solution `shouldBe` "9"

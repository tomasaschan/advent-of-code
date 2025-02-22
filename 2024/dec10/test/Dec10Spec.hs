module Dec10Spec (spec) where

import Dec10
import Test.Hspec

spec :: Spec
spec = describe "Dec 10" $ do
  let input =
        "89010123\n\
        \78121874\n\
        \87430965\n\
        \96549874\n\
        \45678903\n\
        \32019012\n\
        \01329801\n\
        \10456732\n"

  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "36"

  it "should be correct about the trailhead starting at (2,5)" $ do
    let g = interpret input
    let peaks = hike g (2, 5)

    peaks `shouldBe` 1

  it "should solve b" $ do
    snd solution `shouldBe` "81"

--   it "should solve real input" $ do
--     realInput <- readFile "../../inputs/2024/10.txt"
--     solve realInput `shouldBe` ("0", "0")

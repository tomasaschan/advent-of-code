module Dec06Spec (spec) where

import Data.Map as Map
import Dec06
import Test.Hspec

spec :: Spec
spec = describe "Dec 6" $ do
  let input =
        "....#.....\n\
        \.........#\n\
        \..........\n\
        \..#.......\n\
        \.......#..\n\
        \..........\n\
        \.#..^.....\n\
        \........#.\n\
        \#.........\n\
        \......#..."
  let solution = solve input

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/06.txt"
    solve realInput `shouldBe` ("4982", "1663")

  it "should solve a" $ do
    fst solution `shouldBe` "41"

  describe "the road to b" $ do
    let (m, g) = parseInput input
    it "should find no loop in the original input" $ do
      endsInLoop m g `shouldBe` False

    it "should find a loop with the first proposed added obstacle" $ do
      let m' = Map.insert (3, 6) True m
      endsInLoop m' g `shouldBe` True

    it "finds all loop variants" $ do
      countLoops m mempty g `shouldBe` 6

  it "should solve b" $ do
    snd solution `shouldBe` "6"

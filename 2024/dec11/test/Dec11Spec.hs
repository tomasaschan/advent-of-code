module Dec11Spec (spec) where

import Data.Map (Map, fromList, singleton, unionWith)
import Dec11
import Test.Hspec

collate :: [Int] -> Map Int Int
collate = foldr (unionWith (+)) mempty . fmap (`singleton` 1)

spec :: Spec
spec = describe "Dec 11" $ do
  it "iterates once correctly" $ do
    let input = fromList [(0,1), (1,1), (10,1), (99,1), (999,1)]
    let expected = fromList [(1, 2), (2024, 1), (0, 1), (9, 2), (2021976, 1)]
    step input `shouldBe` expected

  describe "longer example" $ do
    it "after 1 blink" $ do
      step (collate [125, 17]) `shouldBe` collate [253000, 1, 7]
    it "after 2 blinks" $ do
      step (collate [253000, 1, 7]) `shouldBe` collate [253, 0, 2024, 14168]
    it "after 3 blinks" $ do
      step (collate [253, 0, 2024, 14168]) `shouldBe` collate [512072, 1, 20, 24, 28676032]
    it "after 4 blinks" $ do
      step (collate [512072, 1, 20, 24, 28676032]) `shouldBe` collate [512, 72, 2024, 2, 0, 2, 4, 2867, 6032]
    it "after 5 blinks" $ do
      step (collate [512, 72, 2024, 2, 0, 2, 4, 2867, 6032]) `shouldBe` collate [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32]
    it "after 6 blinks" $ do
      step (collate [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32]) `shouldBe` collate [2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2]

  let input = "125 17"
  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "55312"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/11.txt"
    solve realInput `shouldBe` ("199753", "239413123020116")

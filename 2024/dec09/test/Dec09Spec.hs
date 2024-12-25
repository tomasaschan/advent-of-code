module Dec09Spec (spec) where

import Dec09
import Test.Hspec

spec :: Spec
spec = describe "Dec 9" $ do
  let input = "2333133121414131402\n"
  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "1928"

-- fst solution `shouldBe` "1928"

--   it "should solve b" $ do
--     snd solution `shouldBe` "0"

-- it "should solve real input" $ do
--   realInput <- readFile "../../inputs/2024/09.txt"
--   solve realInput `shouldBe` ("0", "0")

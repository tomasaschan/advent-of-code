module Dec14Spec (spec) where

import Dec14
import Test.Hspec

spec :: Spec
spec = describe "Dec 14" $ do
  let input =
        "p=0,4 v=3,-3\n\
        \p=6,3 v=-1,-3\n\
        \p=10,3 v=-1,2\n\
        \p=2,0 v=2,-1\n\
        \p=0,0 v=1,3\n\
        \p=3,0 v=-2,-2\n\
        \p=7,6 v=-1,-3\n\
        \p=3,0 v=-1,-2\n\
        \p=9,3 v=2,3\n\
        \p=7,3 v=-1,2\n\
        \p=2,4 v=2,-3\n\
        \p=9,5 v=-3,-3\n"
  let solution = solve' (11, 7) 100 input

  it "should solve a" $ do
    fst solution `shouldBe` "12"

--   it "should solve b" $ do
--     snd solution `shouldBe` "0"

--   it "should solve real input" $ do
--     realInput <- readFile "../../inputs/2024/14.txt"
--     solve realInput `shouldBe` ("0", "0")

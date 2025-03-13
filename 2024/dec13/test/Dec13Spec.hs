module Dec13Spec (spec) where

import Dec13
import Test.Hspec

spec :: Spec
spec = describe "Dec 13" $ do
  let input =
        "Button A: X+94, Y+34\n\
        \Button B: X+22, Y+67\n\
        \Prize: X=8400, Y=5400\n\
        \\n\
        \Button A: X+26, Y+66\n\
        \Button B: X+67, Y+21\n\
        \Prize: X=12748, Y=12176\n\
        \\n\
        \Button A: X+17, Y+86\n\
        \Button B: X+84, Y+37\n\
        \Prize: X=7870, Y=6450\n\
        \\n\
        \Button A: X+69, Y+23\n\
        \Button B: X+27, Y+71\n\
        \Prize: X=18641, Y=10279\n"
  let solution = solve input

  it "should solve a" $ do
    fst solution `shouldBe` "480"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/13.txt"
    solve realInput `shouldBe` ("29187", "99968222587852")

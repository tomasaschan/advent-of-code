module Dec05Spec (spec) where

import Dec05

import Test.Hspec

spec :: Spec
spec = describe "Dec 5" $ do
  let input =
        "47|53\n\
        \97|13\n\
        \97|61\n\
        \97|47\n\
        \75|29\n\
        \61|13\n\
        \75|53\n\
        \29|13\n\
        \97|29\n\
        \53|29\n\
        \61|53\n\
        \97|53\n\
        \61|29\n\
        \47|13\n\
        \75|47\n\
        \97|75\n\
        \47|61\n\
        \75|61\n\
        \47|29\n\
        \75|13\n\
        \53|13\n\
        \\n\
        \75,47,61,53,29\n\
        \97,61,53,29,13\n\
        \75,29,13\n\
        \75,97,47,61,53\n\
        \61,13,29\n\
        \97,13,75,29,47\n"
  let solution = solve input

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/05.txt"
    solve realInput `shouldBe` ("3608", "4922")

  it "should solve a" $ do
    fst solution `shouldBe` "143"

  it "should solve b" $ do
    snd solution `shouldBe` "123"

  describe "correcting order" $ do
    let i = parse input

    it "solves the first example" $ do
      correctOrder (rules i) [75, 97, 47, 61, 53] [] `shouldBe` [97, 75, 47, 61, 53]

    it "solves the second example" $ do
      correctOrder (rules i) [61, 13, 29] [] `shouldBe` [61, 29, 13]

    it "solves the third example" $ do
      correctOrder (rules i) [97, 13, 75, 29, 47] [] `shouldBe` [97, 75, 47, 29, 13]

module Dec08Spec (spec) where

import Dec08
import Test.Hspec

spec :: Spec
spec = describe "Dec 8" $ do
  describe "trivial input" $ do
    let input =
          "..........\n\
          \..........\n\
          \..........\n\
          \....a.....\n\
          \..........\n\
          \.....a....\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n"

    it "should solve a" $ do
      fst (solve input) `shouldBe` "2"

  describe "adding one node" $ do
    let input =
          "..........\n\
          \..........\n\
          \..........\n\
          \....a.....\n\
          \........a.\n\
          \.....a....\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n"
    it "should solve a" $ do
      fst (solve input) `shouldBe` "4"

  describe "T antennae" $ do
    let input =
          "T.........\n\
          \...T......\n\
          \.T........\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n\
          \..........\n"
    it "should solve b" $ do
      snd (solve input) `shouldBe` "9"

  describe "full example" $ do
    let input =
          "............\n\
          \........0...\n\
          \.....0......\n\
          \.......0....\n\
          \....0.......\n\
          \......A.....\n\
          \............\n\
          \............\n\
          \........A...\n\
          \.........A..\n\
          \............\n\
          \............\n"
    let solution = solve input
    it "should solve a" $ do
      fst solution `shouldBe` "14"

    it "should solve b" $ do
      snd solution `shouldBe` "34"

  it "should solve real input" $ do
    realInput <- readFile "../../inputs/2024/08.txt"
    solve realInput `shouldBe` ("426", "1359")

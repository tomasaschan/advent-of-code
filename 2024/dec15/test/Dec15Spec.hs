module Dec15Spec (spec) where

import Dec15
import Test.Hspec

spec :: Spec
spec = describe "Dec 15" $ do
  describe "small example" $ do
    let input =
          "########\n\
          \#..O.O.#\n\
          \##@.O..#\n\
          \#...O..#\n\
          \#.#.O..#\n\
          \#...O..#\n\
          \#......#\n\
          \########\n\
          \\n\
          \<^^>>>vv<v>>v<<\n"
    let solution = solve input

    it "should solve a" $ do
      fst solution `shouldBe` "2028"

  describe "larger example" $ do
    let input =
          "##########\n\
          \#..O..O.O#\n\
          \#......O.#\n\
          \#.OO..O.O#\n\
          \#..O@..O.#\n\
          \#O#..O...#\n\
          \#O..O..O.#\n\
          \#.OO.O.OO#\n\
          \#....O...#\n\
          \##########\n\
          \\n\
          \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v\n\
          \vv^v>^\n\
          \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>\n\
          \>^<v<v\n\
          \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^\n\
          \<^^vv<\n\
          \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^\n\
          \<^^^^^\n\
          \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<\n\
          \<<>^><\n\
          \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v\n\
          \v^v<v^\n\
          \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>\n\
          \^<>vv^\n\
          \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^\n\
          \>^>v<>\n\
          \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>\n\
          \<><<v>\n\
          \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv\n\
          \>v^<<^\n"

    let solution = solve input

    it "should solve a" $ do
      fst solution `shouldBe` "10092"

-- describe "small example" $ do
--   let input =
--     "########\n\
--     \#..O.O.#\n\
--     \##@.O..#\n\
--     \#...O..#\n\
--     \#.#.O..#\n\
--     \#...O..#\n\
--     \#......#\n\
--     \########\n\
--     \\n\
--     \<^^>>>vv<v>>v<<\n"

--   return ()

--   it "should solve b" $ do
--     snd solution `shouldBe` "0"

--   it "should solve real input" $ do
--     realInput <- readFile "../../inputs/2024/15.txt"
--     solve realInput `shouldBe` ("0", "0")

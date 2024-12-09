module Dec04 (
  solve,
  parse,
  Letter (..),
) where

import Data.Map as Map

solve :: String -> (String, String)
solve input = (a, b)
 where
  g = parse input

  a = show $ allWords g

  b = show $ allXmasX g

data Letter = X | M | A | S deriving (Eq, Show)

newtype Grid = Grid {grid :: Map.Map (Int, Int) Letter} deriving (Eq)

word :: Grid -> (Int, Int) -> (Int, Int) -> Letter -> Int
word g (x, y) (dx, dy) X | Map.lookup (x, y) (grid g) == Just X = word g (x + dx, y + dy) (dx, dy) M
word g (x, y) (dx, dy) M | Map.lookup (x, y) (grid g) == Just M = word g (x + dx, y + dy) (dx, dy) A
word g (x, y) (dx, dy) A | Map.lookup (x, y) (grid g) == Just A = word g (x + dx, y + dy) (dx, dy) S
word g (x, y) _ S | Map.lookup (x, y) (grid g) == Just S = 1
word _ _ _ _ = 0

wordsFrom :: Grid -> (Int, Int) -> Int
wordsFrom g start = sum [word g start (dx, dy) X | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

allWords :: Grid -> Int
allWords (Grid g) = sum . fmap (wordsFrom (Grid g)) $ Map.keys g

lookForX :: Grid -> (Int, Int) -> Bool
lookForX g c = aat g c && masGrave g c && masAcute g c

aat :: Grid -> (Int, Int) -> Bool
aat g (x, y) = Map.lookup (x, y) (grid g) == Just A

masGrave :: Grid -> (Int, Int) -> Bool
masGrave (Grid g) (x, y) =
  case (Map.lookup (x - 1, y - 1) g, Map.lookup (x + 1, y + 1) g) of
    (Just M, Just S) -> True
    (Just S, Just M) -> True
    _ -> False
masAcute :: Grid -> (Int, Int) -> Bool
masAcute (Grid g) (x, y) =
  case (Map.lookup (x + 1, y - 1) g, Map.lookup (x - 1, y + 1) g) of
    (Just M, Just S) -> True
    (Just S, Just M) -> True
    _ -> False

allXmasX :: Grid -> Int
allXmasX g = length $ Prelude.filter (lookForX g) $ Map.keys (grid g)

instance Show Grid where
  show g = unlines [concat [maybe " " show (Map.lookup (x, y) (grid g)) | x <- [0 .. width g]] | y <- [0 .. height g]]
   where
    width :: Grid -> Int
    width = maximum . fmap fst . Map.keys . grid
    height :: Grid -> Int
    height = maximum . fmap snd . Map.keys . grid

parse :: String -> Grid
parse = Grid . Map.mapMaybe id . Map.fromList . concat . zipWith row [0 ..] . lines
 where
  letter 'X' = Just X
  letter 'M' = Just M
  letter 'A' = Just A
  letter 'S' = Just S
  letter _ = Nothing
  row y = zip [(x, y) | x <- [0 ..]] . fmap letter

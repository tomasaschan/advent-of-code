module Dec16
  ( solve,
  )
where

import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE (toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe)
import Helpers.Algorithms.Djikstra (djikstra)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = maybe "<no result found>" (show . fst) . findCheapestPath . parse $ input
    b = ""

data Direction = North | East | South | West
  deriving (Show, Eq, Ord)

newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord)

findCheapestPath :: Problem -> Maybe (Int, [Pos])
findCheapestPath problem = do
  (c, path) <- djikstra (neighbors problem) (isDone problem) $ initial (start problem)
  let path' = pos <$> NE.toList path
  return (c, path')

data Problem = Problem {walls :: Set Pos, start :: Pos, end :: Pos} deriving (Show)

isDone :: Problem -> (Pos, Direction) -> Bool
isDone (Problem {end = e}) (p, _) = p == e

neighbors :: (Num c) => Problem -> (Pos, Direction) -> [(c, (Pos, Direction))]
neighbors (Problem {walls = ws}) (p, dir) =
  filter (not . (`Set.member` ws) . pos . snd) $ bimap cost (move (p, dir)) . dupe <$> [Walk, TurnLeft, TurnRight]

initial :: Pos -> (Pos, Direction)
initial p = (p, East)

pos :: (Pos, Direction) -> Pos
pos = fst

data Move = Walk | TurnLeft | TurnRight deriving (Show, Eq)

cost :: (Num c) => Move -> c
cost Walk = 1
cost TurnLeft = 1000
cost TurnRight = 1000

move :: (Pos, Direction) -> Move -> (Pos, Direction)
move (Pos (x, y), dir) Walk =
  let pos' = case dir of
        North -> Pos (x, y + 1)
        East -> Pos (x + 1, y)
        South -> Pos (x, y - 1)
        West -> Pos (x - 1, y)
   in (pos', dir)
move (pos', dir) TurnLeft =
  let dir' = case dir of
        North -> West
        West -> South
        South -> East
        East -> North
   in (pos', dir')
move (pos', dir) TurnRight =
  let dir' = case dir of
        North -> East
        East -> South
        South -> West
        West -> North
   in (pos', dir')

parse :: String -> Problem
parse = (\(w, s, e) -> Problem w s e) . joinAll . fmap (uncurry line) . zip [0 ..] . lines
  where
    line :: Int -> String -> (Set Pos, Maybe Pos, Maybe Pos)
    line y =
      foldl
        ( \(walls', start', end') (x, c) -> case c of
            '#' -> (Set.insert (Pos (x, y)) walls', start', end')
            'S' -> (walls', Just (Pos (x, y)), end')
            'E' -> (walls', start', Just (Pos (x, y)))
            _ -> (walls', start', end')
        )
        (Set.empty, Nothing, Nothing)
        . zip [0 ..]

    joinAll :: [(Set Pos, Maybe Pos, Maybe Pos)] -> (Set Pos, Pos, Pos)
    joinAll =
      foldl
        (\(walls', start', end') (w, s, e) -> (Set.union walls' w, fromMaybe start' s, fromMaybe end' e))
        (Set.empty, Pos (-1, -1), Pos (-1, -1))

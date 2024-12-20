{-# LANGUAGE TupleSections #-}

module Dec08 (
  solve,
) where

import Control.Arrow
import Data.Map as Map
import Data.Maybe
import Data.Set as Set
import Helpers.Combinatorics

solve :: String -> (String, String)
solve input = (a input, b input)
 where
  a = show . countAntinodes . city bothNodes
  b = show . countAntinodes . city allNodes

countAntinodes :: City -> Int
countAntinodes (City _ _ _ ans) = Set.size ans

type Antennae = (Map Char (Set (Int, Int)))
type Antinodes = Set (Int, Int)
data City = City Int Int Antennae Antinodes deriving (Eq)

city :: (Int -> Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]) -> String -> City
city f input = City w h ats ans
 where
  w = length (head (lines input))
  h = length (lines input)

  ats = antennae input
  ans = antinodes (f w h) ats

within :: Int -> Int -> (Int, Int) -> Bool
within w h (x, y) = 0 <= x && x < w && 0 <= y && y < h

antennae :: String -> Antennae
antennae =
  lines
    >>> zip [0 ..]
    >>> fmap (second (Prelude.filter ((/= '.') . snd) . zip [0 ..]))
    >>> concatMap (\(y, xcs) -> fmap (\(x, c) -> (c, (x, y))) xcs)
    >>> fmap (second Set.singleton)
    >>> Map.fromListWith Set.union

antinodes :: (((Int, Int), (Int, Int)) -> [(Int, Int)]) -> Antennae -> Antinodes
antinodes f = Set.fromList . concatMap (concatMap f . pairwise . Set.toList) . Map.elems

bothNodes :: Int -> Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
bothNodes w h ((x1, y1), (x2, y2)) = Prelude.filter (within w h) [(2 * x1 - x2, 2 * y1 - y2), (2 * x2 - x1, 2 * y2 - y1)]

allNodes :: Int -> Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
allNodes w h (a, b) = allNodesUp (a, b) ++ allNodesDown (a, b)
 where
  allNodesUp ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
        dy = y2 - y1
     in takeWhile (within w h) [(x1 - i * dx, y1 - i * dy) | i <- [0 ..]]
  allNodesDown ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
        dy = y2 - y1
     in takeWhile (within w h) [(x2 + i * dx, y2 + i * dy) | i <- [0 ..]]
instance Show City where
  show (City w h a an) = unlines $ fmap row [0 .. h - 1]
   where
    as = Map.fromList . concatMap (\(c, xys) -> Set.toList $ Set.map (,c) xys) . Map.toList $ a

    row y = fmap (`cell` y) [0 .. w - 1]
    cell x y | (x, y) `elem` an = '#'
    cell x y = fromMaybe ' ' $ as Map.!? (x, y)

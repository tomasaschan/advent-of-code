{-# LANGUAGE NamedFieldPuns #-}

module Dec05 (
  solve,
  allCorrectOrder,
  isCorrectOrder,
  correctOrder,
  parse,
  Input (..),
) where

import Data.Bifunctor
import Data.List (groupBy, sortBy)
import Data.List.Split
import Data.Map as Map (Map, fromList, lookup)
import Data.Ord (comparing)
import Data.Set as Set (Set, fromList)

solve :: String -> (String, String)
solve input = (a, b)
 where
  Input{rules, pages} = parse input
  a = show $ sum (middle <$> filter (allCorrectOrder rules) pages)
  b = show $ sum $ middle <$> (fmap (correctOrder rules []) . filter (not . allCorrectOrder rules) $ pages)

data Input = Input {rules :: Map Int (Set Int), pages :: [[Int]]} deriving (Show, Eq)

isCorrectOrder :: Map Int (Set Int) -> Int -> [Int] -> Bool
isCorrectOrder rules x xs = all (\x' -> maybe True (elem x') $ Map.lookup x rules) xs && all (\x' -> maybe True (notElem x) $ Map.lookup x' rules) xs

allCorrectOrder :: Map Int (Set Int) -> [Int] -> Bool
allCorrectOrder _ [] = True
allCorrectOrder rules (x : xs) = isCorrectOrder rules x xs && allCorrectOrder rules xs

correctOrder :: Map Int (Set Int) -> [Int] -> [Int] -> [Int]
correctOrder _ [] [] = []
correctOrder rules [] ys = correctOrder rules ys []
correctOrder rules (x : xs) ys | isCorrectOrder rules x (xs <> ys) = x : correctOrder rules xs ys
correctOrder rules (x : xs) ys = correctOrder rules xs (ys <> [x])

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)

parse :: String -> Input
parse input = Input{rules = rs, pages = ps}
 where
  (rawRules, rawPages) = fmap (filter (/= "")) . break (== "") $ lines input
  rs = Map.fromList . fmap (\rs' -> (fst $ head rs', Set.fromList $ fmap snd rs')) . groupBy (\a b -> fst a == fst b) . sortBy (comparing fst) $ fmap rule rawRules
  ps = fmap (fmap read . splitOn ",") rawPages

  rule = bimap read (read . filter (/= '|')) . break (== '|')

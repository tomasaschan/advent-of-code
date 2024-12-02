module Lib (
    solve,
) where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Either (partitionEithers)
import Data.List (sort)
import Data.Map.Strict (Map (), insertWith, lookup)
import Prelude hiding (lookup)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = case parse input of
        Right ns -> show . sum . fmap distance . uncurry zip . bimap sort sort . unzip $ ns
        Left err -> err

    b = case parse input of
        Right ns -> show . uncurry similarityScore . second buildLookup . unzip $ ns
        Left err -> err

buildLookup :: [Int] -> Map Int Int
buildLookup rhs = _bulidLookup rhs mempty
  where
    _bulidLookup [] acc = acc
    _bulidLookup (x : xs) acc = _bulidLookup xs (insertWith (+) x 1 acc)

similarityScore :: [Int] -> Map Int Int -> Int
similarityScore [] _ = 0
similarityScore (x : xs) rhs =
    case lookup x rhs of
        Just n -> x * n + similarityScore xs rhs
        Nothing -> similarityScore xs rhs

distance :: (Int, Int) -> Int
distance (x, y) = abs (x - y)

parse :: String -> Either String [(Int, Int)]
parse input = if Prelude.null ls then Right rs else Left $ head ls
  where
    (ls, rs) = partitionEithers . fmap (\case [x, y] -> Right (x, y); _ -> Left "invalid input") . map (fmap read . words) . lines $ input

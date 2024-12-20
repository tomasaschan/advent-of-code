{-# LANGUAGE TupleSections #-}

module Helpers.Combinatorics
(
  pairwise,
)
where

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x : xs) = fmap (x,) xs ++ pairwise xs

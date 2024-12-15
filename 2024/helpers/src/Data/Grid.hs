module Data.Grid (World, Grid, parse) where

import Data.Map as Map

type World c a = Map c a
type Grid a = World (Int, Int) a

parse :: (Ord c) => (Int -> Int -> c) -> (Char -> Maybe a) -> String -> World c a
parse coord value = Map.mapMaybe id . Map.fromList . concat . zipWith row [0 ..] . lines
 where
  row y = zip [coord x y | x <- [0 ..]] . fmap value

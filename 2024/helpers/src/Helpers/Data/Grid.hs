{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.Data.Grid
  ( World,
    Grid,
    parse,
    Sparse (..),
    sparse,
    inside,
    neighbors,
    bounds,
    (!),
    (!?),
  )
where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

type World c a = Map c a

type Grid a = World (Int, Int) a

parse :: (Ord c) => (Int -> Int -> c) -> (Char -> Maybe a) -> String -> World c a
parse coord value = Map.mapMaybe id . Map.fromList . concat . zipWith row [0 ..] . lines
  where
    row y = zip [coord x y | x <- [0 ..]] . fmap value

data Sparse a = Sparse Int Int (Grid a) deriving (Eq)

(!) :: Sparse a -> (Int, Int) -> a
(Sparse _ _ g) ! (x, y) = g Map.! (x, y)

(!?) :: Sparse a -> (Int, Int) -> Maybe a
(Sparse _ _ g) !? (x, y) = g Map.!? (x, y)

sparse :: (Char -> Maybe a) -> String -> Sparse a
sparse f input = Sparse w h g
  where
    w = length (head (lines input))
    h = length (lines input)
    g = parse (,) f input

inside :: Sparse a -> (Int, Int) -> Bool
inside (Sparse w h _) (x, y) = 0 <= x && x < w && 0 <= y && y < h

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx == 0) /= (dy == 0)]

bounds :: Sparse a -> (Int, Int, Int, Int)
bounds (Sparse w h _) = (0, w - 1, 0, h - 1)

instance {-# OVERLAPPING #-} Show (Sparse String) where
  show = _show id

instance {-# OVERLAPPING #-} Show (Sparse Char) where
  show = _show (: [])

instance (Show a) => Show (Sparse a) where
  show = _show show

_show :: (a -> String) -> Sparse a -> String
_show f (Sparse w h g) = unlines $ fmap row [0 .. h - 1]
  where
    row y = intercalate "" $ fmap (\x -> maybe " " f (Map.lookup (x, y) g)) [0 .. w - 1]

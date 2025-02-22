module Dec10
  ( solve,
    hike,
    interpret,
  )
where

import Data.Bifunctor (bimap)
import Data.List (singleton)
import Data.Maybe (mapMaybe)
import Data.Sequence
  ( Seq (Empty, (:<|)),
    fromList,
    singleton,
    (><),
  )
import Data.Set (Set, empty, insert, notMember)
import Data.Tuple.Extra (dupe)
import Helpers.Data.Grid (Sparse, bounds, neighbors, sparse, (!), (!?))
import Text.Read (readMaybe)

solve :: String -> (String, String)
solve = bimap a b . dupe
  where
    a input = show . sum . fmap (hike g) . trailheads $ g
      where
        g = interpret input
    b = const ""

interpret :: String -> Sparse Int
interpret = sparse (readMaybe . Data.List.singleton)

trailheads :: Sparse Int -> [(Int, Int)]
trailheads g = [(x, y) | x <- [xlo .. xhi], y <- [ylo .. yhi], g ! (x, y) == 0]
  where
    (xlo, xhi, ylo, yhi) = bounds g

nextFrom :: Sparse Int -> (Int, Int) -> [(Int, Int)]
nextFrom g c = mapMaybe f $ neighbors c
  where
    f c' = do
      v <- g !? c'
      if (v - g ! c) == 1
        then Just c'
        else Nothing

hike :: Sparse Int -> (Int, Int) -> Int
hike g c = length $ bfs g empty empty (Data.Sequence.singleton c)

bfs :: Sparse Int -> Set (Int, Int) -> Set (Int, Int) -> Seq (Int, Int) -> Set (Int, Int)
bfs _ peaks _ Empty = peaks
bfs g peaks visited (c :<| q) = bfs g peaks' visited' q'
  where
    h = g ! c
    peaks' = if h == 9 then insert c peaks else peaks
    visited' = insert c visited
    q'' = fromList $ filter (`notMember` visited) $ nextFrom g c
    q' = q >< q''

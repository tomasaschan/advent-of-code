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
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe)
import Helpers.Data.Grid (Sparse, bounds, neighbors, sparse, (!), (!?))
import Text.Read (readMaybe)

solve :: String -> (String, String)
solve = bimap a b . dupe
  where
    a = show . score . hikeAll . interpret
    b = show . rating . hikeAll . interpret

interpret :: String -> Sparse Int
interpret = sparse (readMaybe . Data.List.singleton)

score :: [[(Int, Int)]] -> Int
score = sum . fmap (length . Set.fromList)

rating :: [[(Int, Int)]] -> Int
rating = sum . fmap length

hikeAll :: Sparse Int -> [[(Int, Int)]]
hikeAll g = fmap (hike g) . trailheads $ g

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

hike :: Sparse Int -> (Int, Int) -> [(Int, Int)]
hike g c = bfs g [] (Data.Sequence.singleton c)

bfs :: Sparse Int -> [(Int, Int)] -> Seq (Int, Int) -> [(Int, Int)]
bfs _ peaks Empty = peaks
bfs g peaks (c :<| q) = bfs g peaks' q'
  where
    h = g ! c
    peaks' = if h == 9 then c : peaks else peaks
    q'' = fromList $ nextFrom g c
    q' = q >< q''

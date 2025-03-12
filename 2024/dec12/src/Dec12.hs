module Dec12
  ( solve,
    parse',
    findPlots',
    sides,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map ((!), (!?))
import qualified Data.Map as Map
import Data.Sequence (Seq (..), fromList, (><))
import Data.Set (Set)
import qualified Data.Set as Set (difference, fold, fromList, insert, lookupMin)
import Data.Tuple.Extra (dupe)
import Helpers.Data.Grid (Grid, World (..), neighbors, parse)

solve :: String -> (String, String)
solve input = (a, b)
  where
    plots = fmap fst . findPlots' . parse' $ input
    a = show . sum . fmap (price perimeter) $ plots
    b = show . sum . fmap (price sides) $ plots

price :: (Plot -> Int) -> Plot -> Int
price sideCost = uncurry (*) . bimap area sideCost . dupe

type Garden = Grid Char

type Plot = Grid ()

type C = (Int, Int)

parse' :: String -> Garden
parse' = parse (,) Just

findPlots' :: Garden -> [(Plot, Char)]
findPlots' (W g) = findPlots (W g) (Set.fromList $ Map.keys g) mempty

findPlots :: Garden -> Set C -> [(Plot, Char)] -> [(Plot, Char)]
findPlots (W g) soil plots =
  case Set.lookupMin soil of
    Nothing -> plots
    Just start ->
      let plant = g ! start
          plot = findOnePlot' (W g) start
          soil' = Set.difference soil plot
          plot' = W $ Set.fold (`Map.insert` ()) mempty plot
       in findPlots (W g) soil' ((plot', plant) : plots)

findOnePlot' :: Garden -> C -> Set C
findOnePlot' g start = findOnePlot g mempty (start :<| Empty)

findOnePlot :: Garden -> Set C -> Seq C -> Set C
findOnePlot _ seen Empty = seen
findOnePlot (W g) seen (next :<| rest) =
  if next `elem` seen
    then findOnePlot (W g) seen rest
    else
      let seen' = Set.insert next seen
          within = filter (((g !? next) ==) . (g !?)) $ neighbors next
          rest' = rest >< fromList (filter (`notElem` seen') within)
       in findOnePlot (W g) seen' rest'

area :: Plot -> Int
area (W g) = length g

perimeter :: Plot -> Int
perimeter (W g) =
  let coords = Map.keys g
      edges c = length . filter ((/= Just ()) . (g !?)) $ neighbors c
   in sum . fmap edges $ coords

sides :: Plot -> Int
sides (W p) =
  let coords = Map.keys p
   in sum $ fmap (cornerCount (W p)) coords

cornerCount :: Plot -> C -> Int
cornerCount (W p) (x, y) =
  -- corner cases are "looking left, kitty-corner, looking right"
  -- oriented for each diagonal direction

  let cornerCases =
        [ [(x - 1, y), (x - 1, y - 1), (x, y - 1)], -- looking nw
          [(x, y - 1), (x + 1, y - 1), (x + 1, y)], -- looking ne
          [(x + 1, y), (x + 1, y + 1), (x, y + 1)], -- looking se
          [(x, y + 1), (x - 1, y + 1), (x - 1, y)] -- looking sw
        ]
      isCorner' cornerCase =
        case fmap (`Map.member` p) cornerCase of
          [False, _, False] -> True
          [True, False, True] -> True
          _ -> False
   in length $ filter isCorner' cornerCases

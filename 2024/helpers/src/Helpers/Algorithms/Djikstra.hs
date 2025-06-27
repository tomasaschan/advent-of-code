module Helpers.Algorithms.Djikstra
  ( djikstra,
    djikstrall,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.PQueue.Min (MinQueue (..), singleton)
import qualified Data.PQueue.Min as PQ

newtype (Num c) => Item c a = Item (c, NonEmpty a) deriving (Show, Eq)

instance (Ord c, Num c, Ord a) => Ord (Item c a) where
  compare (Item (c1, a1 :| _)) (Item (c2, a2 :| _)) = compare (c1, a1) (c2, a2)

djikstra :: (Num c, Ord a, Ord c, Foldable t) => (a -> t (c, a)) -> (a -> Bool) -> a -> Maybe (c, NonEmpty a)
djikstra neighbors isDone start = djikstra' (initial start) Map.empty
  where
    djikstra' Empty _ = Nothing
    djikstra' (Item (cost, current :| path) :< _) _ | isDone current = Just (cost, current :| path)
    djikstra' (Item (cost, current :| path) :< q) seen = djikstra' q' seen'
      where
        (seen', q') = requeue (<) neighbors cost current path q seen

djikstrall :: (Num c, Ord a, Ord c, Foldable t) => (a -> t (c, a)) -> (a -> Bool) -> a -> Maybe (c, [NonEmpty a])
djikstrall neighbors isDone start = djikstrall' (initial start) Map.empty Nothing
  where
    djikstrall' Empty _ result = result
    djikstrall' (Item (cost, _) :< q) seen (Just (cheapest, found))
      | cost > cheapest =
          djikstrall' q seen (Just (cheapest, found))
    djikstrall' (Item (cost, current :| path) :< q) seen Nothing
      | isDone current =
          djikstrall' q seen (Just (cost, [current :| path]))
    djikstrall' (Item (cost, current :| path) :< q) seen (Just (cheapest, found))
      | isDone current && cost == cheapest =
          djikstrall' q seen (Just (cheapest, (current :| path) : found))
    djikstrall' (Item (cost, current :| path) :< q) seen result =
      djikstrall' q' seen' result
      where
        (seen', q') = requeue (<=) neighbors cost current path q seen

initial :: (Num c) => a -> MinQueue (Item c a)
initial start = singleton (Item (0, start :| []))

requeue :: (Ord a, Ord c, Num c, Foldable t) => (c -> c -> Bool) -> (a -> t (c, a)) -> c -> a -> [a] -> MinQueue (Item c a) -> Map.Map a c -> (Map.Map a c, MinQueue (Item c a))
requeue condition neighbors cost current path q seen = (seen', q')
  where
    (seen', q') = foldl step (seen, q) (neighbors current)

    step (seen'', q'') (cost', neighbor) =
      let cost'' = cost + cost'
       in case Map.lookup neighbor seen'' of
            Nothing -> (Map.insert neighbor cost'' seen'', PQ.insert (Item (cost'', neighbor :| current : path)) q'')
            Just cost'''
              | condition cost'' cost''' ->
                  (Map.insert neighbor cost'' seen'', PQ.insert (Item (cost'', neighbor :| current : path)) q'')
            _ -> (seen'', q'')

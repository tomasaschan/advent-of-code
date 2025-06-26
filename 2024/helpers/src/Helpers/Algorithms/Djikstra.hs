module Helpers.Algorithms.Djikstra
  ( djikstra,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.PQueue.Min (MinQueue (..), singleton)
import qualified Data.PQueue.Min as PQ

newtype (Num c) => Item c a = Item (c, NonEmpty a) deriving (Show, Eq)

instance (Ord c, Num c, Ord a) => Ord (Item c a) where
  compare (Item (c1, a1 :| _)) (Item (c2, a2 :| _)) = compare (c1, a1) (c2, a2)

djikstra :: (Num c, Ord a, Ord c) => (a -> [(c, a)]) -> (a -> Bool) -> a -> Maybe (c, NonEmpty a)
djikstra neighbors isDone start = djikstra' (singleton start') Map.empty
  where
    start' = Item (0, start :| [])

    -- djikstra' :: (State a) => MinQueue (Item a) -> Map.Map b Int -> Maybe a
    -- djikstra' :: (Num c) => MinQueue (Item c a) -> Map a c -> Maybe (c, NonEmpty a)
    djikstra' Empty _ = Nothing
    djikstra' q _ | PQ.size q > 1000 = error "queue too large"
    djikstra' (Item (cost, current :| path) :< _) _ | isDone current = Just (cost, current :| path)
    djikstra' (Item (cost, current :| path) :< q) seen = djikstra' q' seen'
      where
        (seen', q') = foldl step (seen, q) (neighbors current)

        step (seen'', q'') (cost', neighbor) =
          let cost'' = cost + cost'
           in case Map.lookup neighbor seen'' of
                Nothing -> (Map.insert neighbor cost'' seen'', PQ.insert (Item (cost'', neighbor :| current : path)) q'')
                Just cost'''
                  | cost''' > cost'' ->
                      (Map.insert neighbor cost'' seen'', PQ.insert (Item (cost'', neighbor :| current : path)) q'')
                _ -> (seen'', q'')

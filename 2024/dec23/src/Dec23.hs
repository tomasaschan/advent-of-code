module Dec23
  ( solve,
  )
where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (intercalate, sort)
import Data.Map (Map, insertWith, keys, (!))
import Data.Ord (comparing)
import Data.Set (Set, difference, fromList, insert, intersection, singleton, toList, union)

solve :: String -> (String, String)
solve input = (a, b)
  where
    network = parse input
    lanParties = bronKerbosch network mempty mempty (fromList $ keys network) mempty

    a = show . length . filter (any (hasPrefix 't')) . toList . partiesOfThree $ lanParties
    b = intercalate "," . sort . toList . maximumBy (comparing length) $ lanParties

type Computer = String

type Network = Map Computer (Set Computer)

partiesOfThree :: Set (Set Computer) -> Set (Set Computer)
partiesOfThree = fromList . concatMap include . toList
  where
    include :: Set Computer -> [Set Computer]
    include party | length party < 3 = []
    include party | length party == 3 = [party]
    include party = fromList <$> subsets 3 (toList party)

    subsets :: Int -> [Computer] -> [[Computer]]
    subsets 0 _ = [[]]
    subsets _ [] = []
    subsets n (x : xs) = fmap (x :) (subsets (n - 1) xs) ++ subsets n xs

hasPrefix :: Char -> String -> Bool
hasPrefix _ "" = False
hasPrefix c (x : _) = c == x

bronKerbosch :: Network -> Set (Set Computer) -> Set Computer -> Set Computer -> Set Computer -> Set (Set Computer)
bronKerbosch network found r p x
  | null p && null x = insert r found
  | otherwise =
      let pivot = maximumBy (compare `on` (length . (network !))) (p `union` x)
          pivoted = p `difference` (network ! pivot)
       in found `union` foldMap f (toList pivoted)
  where
    f v = bronKerbosch network found (insert v r) (p `intersection` (network ! v)) (x `intersection` (network ! v))

parse :: String -> Network
parse input = foldl addConnection mempty $ parseLine <$> lines input
  where
    addConnection acc (from, to) = insertWith union from (singleton to) $ insertWith union to (singleton from) acc

    parseLine line =
      case break (== '-') line of
        (from, '-' : to) -> (from, to)
        _ -> error $ "Invalid line format: " ++ line

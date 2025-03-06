module Dec11
  ( solve,
    step,
  )
where

import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Map (Map, insertWith, singleton, toList, unionWith)
import Data.Tuple.Extra (dupe)
import GHC.TypeNats (Nat)
import Helpers.Data.Int

solve :: String -> (String, String)
solve = bimap a b . dupe
  where
    a = show . sum . stepN 25 . parse
    b = show . sum . stepN 75 . parse

parse :: String -> Map Int Int
parse = counts . map read . words

counts :: [Int] -> Map Int Int
counts = foldr (\n -> insertWith (+) n 1) mempty

evolve :: Int -> Map Int Int
evolve 0 = singleton 1 1
evolve n | even nd = unionWith (+) (singleton a 1) (singleton b 1)
  where
    nd = digits n
    ds = show n
    a = read $ take (div nd 2) ds
    b = read $ drop (div nd 2) ds
evolve n = singleton (n * 2024) 1

step :: Map Int Int -> Map Int Int
step = foldr (unionWith (+) . (f . first evolve)) mempty . toList
  where
    f (m, n) = fmap (* n) m

stepN :: Nat -> Map Int Int -> Map Int Int
stepN 0 xs = xs
stepN n xs = stepN (n - 1) (step xs)

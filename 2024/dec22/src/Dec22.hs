module Dec22
  ( solve,
    prune,
    evolve,
    price,
  )
where

import Data.Bits (xor)
import Data.Map (Map, alter, elems, unionsWith)

solve :: String -> (String, String)
solve input = (a initials, b initials)
  where
    initials = parse input
    a = show . sum . fmap ((!! 2000) . iterate evolve)
    b = show . maximum . elems . unionsWith (+) . fmap pricesByChanges

parse :: String -> [Int]
parse = fmap read . lines

mixF :: (Int -> Int) -> Int -> Int
mixF f n = n `xor` f n

prune :: Int -> Int
prune = (`mod` 16777216)

price :: Int -> Int
price = (`mod` 10)

evolve :: Int -> Int
evolve = prune . mixF (* 2048) . prune . mixF (`div` 32) . prune . mixF (* 64)

type Key = Int

key :: (Int, Int, Int, Int) -> Key
key (a, b, c, d) = ((19 :: Int) ^ (3 :: Int)) * (a + 9) + ((19 :: Int) ^ (2 :: Int)) * (b + 9) + 19 * (c + 9) + (d + 9)

pricesByChanges :: Int -> Map Key Int
pricesByChanges start = build'''' 2000 start mempty
  where
    build'''' k n = build''' (k - 1) (evolve n) (price (evolve n) - price n)
    build''' k n = build'' (k - 1) (evolve n) (price (evolve n) - price n)
    build'' k n = build' (k - 1) (evolve n) (price (evolve n) - price n)
    build' k n = build (k - 1) (evolve n) (price (evolve n) - price n)

    build :: Int -> Int -> Int -> Int -> Int -> Int -> Map Key Int -> Map Key Int
    build 0 _ _ _ _ _ acc = acc
    build k n d c b a acc =
      build (k - 1) n' d' d c b acc'
      where
        n' = evolve n
        p = price n
        p' = price n'
        d' = p' - p
        k' = key (a, b, c, d)
        acc' = alter f k' acc

        f = maybe (Just p) Just

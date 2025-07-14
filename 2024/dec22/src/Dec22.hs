module Dec22
  ( solve,
    mix,
    prune,
    evolve,
    price,
  )
where

import Data.Bits (xor)

solve :: String -> (String, String)
solve input = (a, b)
  where
    initials = parse input
    a = show . sum . fmap (evolveN 2000) $ initials
    b = ""

evolveN :: Int -> Int -> Int
evolveN 0 n = n
evolveN n' n = evolveN (n' - 1) (evolve n)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

price :: Int -> Int
price n = n `mod` 10

evolve :: Int -> Int
evolve n =
  let n' = prune $ mix (n * 64) n
      n'' = prune $ mix (n' `div` 32) n'
      n''' = prune $ mix (n'' * 2048) n''
   in n'''

parse :: String -> [Int]
parse = fmap read . lines

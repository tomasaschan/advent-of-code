module Dec07 (
  solve,
  (<||>),
) where

import Helpers.Data.Int (digits)

solve :: String -> (String, String)
solve input = (show a, show b)
 where
  parsed = fmap parseOne . lines $ input
  a = totalCalibrationResult (isPossiblyTrue [(+), (*)]) parsed
  b = totalCalibrationResult (isPossiblyTrue [(+), (*), (<||>)]) parsed

totalCalibrationResult :: (Int -> Int -> [Int] -> Bool) -> [(Int, [Int])] -> Int
totalCalibrationResult f = sum . fmap fst . filter (uncurry $ f 0)

isPossiblyTrue :: [Int -> Int -> Int] -> Int -> Int -> [Int] -> Bool
isPossiblyTrue _ acc target [] = target == acc
isPossiblyTrue _ acc target (_ : _) | target < acc = False
isPossiblyTrue ops acc target (x : xs) = any (\op -> isPossiblyTrue ops (op acc x) target xs) ops

(<||>) :: Int -> Int -> Int
a <||> b = a * (10 ^ digits b) + b

parseOne :: String -> (Int, [Int])
parseOne s = (read a, fmap read . words . tail $ b)
 where
  (a, b) = break (== ':') s

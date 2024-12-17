module Dec07 (
  solve,
  (<||>),
) where

import Helpers.Data.Int (digits)

solve :: String -> (String, String)
solve input = (show a, show b)
 where
  parsed = fmap parseOne . lines $ input
  a = totalCalibrationResult isPossiblyTrue parsed
  b = totalCalibrationResult isPossiblyTrueWithConcat parsed

totalCalibrationResult :: (Int -> Int -> [Int] -> Bool) -> [(Int, [Int])] -> Int
totalCalibrationResult f = sum . fmap fst . filter (uncurry $ f 0)

isPossiblyTrue :: Int -> Int -> [Int] -> Bool
isPossiblyTrue acc target [] = target == acc
isPossiblyTrue acc target (_ : _) | target < acc = False
isPossiblyTrue acc target (x : xs) = isPossiblyTrue (acc + x) target xs || isPossiblyTrue (acc * x) target xs

isPossiblyTrueWithConcat :: Int -> Int -> [Int] -> Bool
isPossiblyTrueWithConcat acc target [] = target == acc
isPossiblyTrueWithConcat acc target (_ : _) | target < acc = False
isPossiblyTrueWithConcat acc target (x : xs) = isPossiblyTrueWithConcat (acc + x) target xs || isPossiblyTrueWithConcat (acc * x) target xs || isPossiblyTrueWithConcat (acc <||> x) target xs

(<||>) :: Int -> Int -> Int
a <||> b = a' + b
 where
  m :: Int
  m = digits b
  a' = a * (10 ^ m)

parseOne :: String -> (Int, [Int])
parseOne s = (read a, fmap read . words . tail $ b)
 where
  (a, b) = break (== ':') s

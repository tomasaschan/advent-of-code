module Dec02 (
    safe,
    dampen,
    solve,
) where

solve :: String -> (String, String)
solve input = (a, b)
  where
    parsed = parse input
    _solve f = show . length . filter (safe . f) $ parsed
    a = _solve id
    b = _solve dampen

dampen :: [Int] -> [Int]
dampen (x : y : rest) | x == y = dampen (y : rest)
dampen (x : y : z : rest) | x < y && y > z && x < z && z - x <= 3 = x  : dampen (z : rest)
dampen (x : y : z : rest) | x > y && y < z && z > x && x - z <= 3 = x : dampen (z : rest)
dampen (x : xs) = x : dampen xs
dampen [] = []

increasing :: [Int] -> Bool
increasing = all (> 0) . diffs
decreasing :: [Int] -> Bool
decreasing = all (< 0) . diffs
withinLimit :: [Int] -> Bool
withinLimit = all (\x -> abs x <= 3) . diffs

safe :: [Int] -> Bool
safe xs = withinLimit xs && (increasing xs || decreasing xs)

diffs :: [Int] -> [Int]
diffs (x : y : zs) = y - x : diffs (y : zs)
diffs _ = []

parse :: String -> [[Int]]
parse = fmap (fmap read . words) . lines

module Dec14
  ( solve,
    solve',
    step,
    parse',
    isChristmasTree,
  )
where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec

solve :: String -> (String, String)
solve = solve' (101, 103) 100

solve' :: (Int, Int) -> Int -> String -> (String, String)
solve' (x, y) t input = (a input, b input)
  where
    a = show . safetyFactor (x, y) . fmap (step (x, y) t) . parse'
    b = show . stepUntilChristmasTree (x, y) 10000 0 . parse' --   drawAll (x, y) . fmap (step (x, y) 7344)

data Robot = Robot (Int, Int) (Int, Int) deriving (Eq, Ord)

instance Show Robot where
  show (Robot p v) = "p=" ++ show p ++ " v=" ++ show v

robotsIn :: ((Int, Int), (Int, Int)) -> [Robot] -> Int
robotsIn ((xlo, xhi), (ylo, yhi)) = length . filter (\(Robot (x, y) _) -> xlo <= x && x <= xhi && ylo <= y && y <= yhi)

safetyFactor :: (Int, Int) -> [Robot] -> Int
safetyFactor (x, y) rs = product . fmap (`robotsIn` rs) $ quadrants
  where
    xmid = (x - 1) `div` 2
    ymid = (y - 1) `div` 2

    quadrants =
      [ ((0, xmid - 1), (0, ymid - 1)),
        ((xmid + 1, x), (0, ymid - 1)),
        ((0, xmid - 1), (ymid + 1, y)),
        ((xmid + 1, x), (ymid + 1, y))
      ]

step :: (Int, Int) -> Int -> Robot -> Robot
step (x, y) t (Robot (px, py) (vx, vy)) = Robot ((px + t * vx) `mod` x, (py + t * vy) `mod` y) (vx, vy)

parse' :: String -> [Robot]
parse' = either (error . show) id . parse robots ""

integer :: GenParser Char st Int
integer = do
  neg <- option "" (string "-")
  digits <- many1 digit
  return $ read (neg ++ digits)

robot :: GenParser Char st Robot
robot = do
  px <- string "p=" >> integer
  py <- char ',' >> integer
  vx <- string " v=" >> integer
  vy <- char ',' >> integer
  return $ Robot (px, py) (vx, vy)

robots :: GenParser Char st [Robot]
robots = many1 (robot <* newline)

isChristmasTree :: [Robot] -> Bool
isChristmasTree rs = medianDiff == 1
  where
    sorted = sortBy (comparing $ \(Robot (x, y) _) -> (x, y)) rs
    pairs = zip sorted (tail sorted)
    diffs = fmap (\(Robot (x1, y1) _, Robot (x2, y2) _) -> abs (y2 - y1) + x2 - x1) pairs
    sortedDiffs = sort diffs
    medianDiff = sortedDiffs !! (length sortedDiffs `div` 2)

stepUntilChristmasTree :: (Int, Int) -> Int -> Int -> [Robot] -> Int
stepUntilChristmasTree bounds tmax t rs
  | isChristmasTree rs = t
  | t >= tmax = -1
  | otherwise = stepUntilChristmasTree bounds tmax (t + 1) (fmap (step bounds 1) rs)

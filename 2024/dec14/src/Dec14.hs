module Dec14
  ( solve,
    solve',
  )
where

import Data.List (intercalate)
import Text.ParserCombinators.Parsec

solve :: String -> (String, String)
solve = solve' (101, 103) 100

solve' :: (Int, Int) -> Int -> String -> (String, String)
solve' (x, y) t input = (a input, b input)
  where
    a = show . safetyFactor (x, y) . fmap (step (x, y) t) . parse'
    b = const ""

data Robot = Robot (Int, Int) (Int, Int) deriving (Eq)

instance Show Robot where
  show (Robot p v) = "p=" ++ show p ++ " v=" ++ show v

draw :: (Int, Int) -> [Robot] -> String
draw (x, y) rs = unlines $ fmap row [0 .. y]
  where
    row y' = intercalate "" [at x' y' | x' <- [0 .. x]]
    at x' y' = show . length . filter (\(Robot (x'', y'') _) -> x'' == x' && y'' == y') $ rs

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

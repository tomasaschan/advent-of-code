module Dec09 (solve) where

import Data.Bifunctor (Bifunctor (second))
import Data.List (intercalate)

solve :: String -> (String, String)
solve input = (a input, b)
 where
  a = show . checksum . shiftBlocks . parse
  b = ""

data Segment = File Int Int | Empty Int deriving (Eq)

instance Show Segment where
  show (Empty n) = intercalate "" $ replicate n "."
  show (File n i) = intercalate "" $ replicate n (show i)

parse :: String -> [Segment]
parse = _parse True 0 . filter (/= '\n')
 where
  _parse _ _ "" = []
  _parse False i (x : xs) = Empty (read [x]) : _parse True i xs
  _parse True i (x : xs) = File (read [x]) i : _parse False (i + 1) xs

shiftBlocks :: [Segment] -> [Segment]
shiftBlocks blocks = maybe blocks shiftBlocks (shift1 blocks)

shift1 :: [Segment] -> Maybe [Segment]
shift1 disk = do
  (n, i, rest) <- pickUpBlock (reverse disk)
  dropOffBlock (n, i, reverse rest)

pickUpBlock :: [Segment] -> Maybe (Int, Int, [Segment])
pickUpBlock [] = Nothing
pickUpBlock (Empty n : xs) = pickUpBlock xs >>= Just . second (Empty n :)
pickUpBlock (File 1 i : xs) = if hasContinuousEmpty 1 xs then Just (1, i, prependEmpty 1 xs) else Nothing
pickUpBlock (File n i : xs) = if hasContinuousEmpty 1 xs then Just (1, i, prependEmpty 1 $ File (n - 1) i : xs) else Nothing

prependEmpty :: Int -> [Segment] -> [Segment]
prependEmpty n (Empty m : xs) = Empty (n + m) : xs
prependEmpty n xs = Empty n : xs

hasContinuousEmpty :: Int -> [Segment] -> Bool
hasContinuousEmpty _ [] = False
hasContinuousEmpty n (Empty m : _) | m >= n = True
hasContinuousEmpty n (Empty _ : xs) = hasContinuousEmpty n xs
hasContinuousEmpty n (File _ _ : xs) = hasContinuousEmpty n xs

dropOffBlock :: (Int, Int, [Segment]) -> Maybe [Segment]
dropOffBlock (n, i, Empty m : xs) | m > n = Just $ File n i : Empty (m - n) : xs
dropOffBlock (n, i, Empty m : xs) | m == n = Just $ File n i : xs
dropOffBlock (n, i, Empty m : xs) = dropOffBlock (n, i, xs) >>= Just . (Empty m :)
dropOffBlock (n, i, File m j : xs) = dropOffBlock (n, i, xs) >>= Just . (File m j :)
dropOffBlock (_, _, []) = Nothing

checksum :: [Segment] -> Int
checksum = sum . fmap check . zip [0 ..] . blocks
 where
  blocks [] = []
  blocks (Empty n : xs) = replicate n Nothing <> blocks xs
  blocks (File n i : xs) = replicate n (Just i) <> blocks xs

  check (_, Nothing) = 0
  check (i, Just j) = i * j

module Dec09 (solve) where

import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Data.Sequence as S

solve :: String -> (String, String)
solve input = (a input, b)
  where
    a = show . checksum . shiftBlocks . parse
    b = ""

data Segment = File Int Int | Free Int deriving (Eq)

instance Show Segment where
  show (Free n) = intercalate "" $ Prelude.replicate n "."
  show (File n i) = intercalate "" $ Prelude.replicate n (show i)

showS :: Seq Segment -> String
showS = intercalate "" . toList . fmap show

len :: Segment -> Int
len (Free n) = n
len (File n _) = n

parse :: String -> Seq Segment
parse = S.filter ((/= 0) . len) . _parse True 0 . Prelude.filter (/= '\n')
  where
    _parse _ _ "" = Empty
    _parse False i (x : xs) = Free (read [x]) :<| _parse True i xs
    _parse True i (x : xs) = File (read [x]) i :<| _parse False (i + 1) xs

shiftBlocks :: Seq Segment -> Seq Segment
shiftBlocks blocks = shiftBlocks' blocks Empty Empty

shiftBlocks' :: Seq Segment -> Seq Segment -> Seq Segment -> Seq Segment
-- done!
shiftBlocks' Empty prefix suffix = prefix >< S.reverse suffix
-- empty at end, shift to suffix
shiftBlocks' (blocks :|> Free n) prefix suffix = shiftBlocks' blocks prefix (Free n :<| suffix)
-- file at start, shift to prefix
shiftBlocks' (File n d :<| blocks) prefix suffix = shiftBlocks' blocks (prefix :|> File n d) suffix
-- empty at start, file at end, room for more; shift file from end to prefix, pad with leftover space
shiftBlocks' ((Free n :<| blocks) :|> File m d) prefix suffix | n > m = shiftBlocks' (Free (n - m) :<| blocks) (prefix :|> File m d) (Free n :<| suffix)
-- empty at start, file at end, no room for all; shift what fits and leave the rest at end
shiftBlocks' ((Free n :<| blocks) :|> File m d) prefix suffix | n < m = shiftBlocks' (blocks :|> File (m - n) d) (prefix :|> File n d) (Free n :<| suffix)
-- empty at start, file at end, exact fit; shift all
shiftBlocks' ((Free n :<| blocks) :|> File _ d) prefix suffix = shiftBlocks' blocks (prefix :|> File n d) (Free n :<| suffix)
shiftBlocks' (Free n :<| blocks) prefix suffix = shiftBlocks' blocks (prefix :|> Free n) suffix

checksum :: Seq Segment -> Int
checksum = sum . fmap check . Prelude.zip [0 ..] . blocks
  where
    blocks :: Seq Segment -> [Maybe Int]
    blocks Empty = []
    blocks (Free n :<| xs) = Prelude.replicate n Nothing <> blocks xs
    blocks (File n i :<| xs) = Prelude.replicate n (Just i) <> blocks xs

    check (_, Nothing) = 0
    check (i, Just j) = i * j

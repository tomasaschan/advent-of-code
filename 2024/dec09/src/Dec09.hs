module Dec09 (solve) where

import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Sequence as S
import Data.Tuple.Extra (dupe)
import Debug.Trace

solve :: String -> (String, String)
solve = bimap a b . dupe
  where
    a = show . checksum . shiftBlocks . parse
    b = show . checksum . shiftFiles . parse

data Segment = File Int Int | Free Int deriving (Eq)

instance Show Segment where
  show (Free n) = intercalate "" $ Prelude.replicate n "."
  show (File n i) = intercalate "" $ Prelude.replicate n (show i)

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

shiftFiles :: Seq Segment -> Seq Segment
shiftFiles = shiftFiles' Empty Empty

shiftFiles' :: Seq Segment -> Seq Segment -> Seq Segment -> Seq Segment
shiftFiles' prefix suffix Empty = prefix >< suffix
-- just one file left; it apparently could not be shifted, so move it to suffix
shiftFiles' prefix suffix (File n d :<| Empty) = shiftFiles' Empty (File n d :<| suffix) prefix
-- file at start, shift to prefix
shiftFiles' prefix suffix (File n d :<| segments) = shiftFiles' (prefix :|> File n d) suffix segments
-- empty at end, shift to suffix
shiftFiles' prefix suffix (segments :|> Free n) = shiftFiles' prefix (Free n :<| suffix) segments
-- more empty at start than file at end; shift file and start over
shiftFiles' prefix suffix ((Free n :<| segments) :|> File m d) | n > m = shiftFiles' Empty (Free m :<| suffix) ((prefix :|> File m d :|> Free (n - m)) >< segments)
-- less empty at start than file at end; shift space to prefix and move on
shiftFiles' prefix suffix ((Free n :<| segments) :|> File m d) | n < m = shiftFiles' (prefix :|> Free n) suffix (segments :|> File m d)
-- exact fit; shift file and start over
shiftFiles' prefix suffix ((Free n :<| segments) :|> File m d) | m == n = shiftFiles' Empty (Free n :<| suffix) ((prefix :|> File m d) >< segments)
shiftFiles' prefix suffix segments = trace "unmatched state:" (const $ const $ const Empty) prefix suffix segments

-- shiftFiles' prefix suffix (Free n :<| blocks) = shiftFiles' (prefix :|> Free n) suffix blocks

checksum :: Seq Segment -> Int
checksum = sum . fmap check . Prelude.zip [0 ..] . blocks
  where
    blocks :: Seq Segment -> [Maybe Int]
    blocks Empty = []
    blocks (Free n :<| xs) = Prelude.replicate n Nothing <> blocks xs
    blocks (File n i :<| xs) = Prelude.replicate n (Just i) <> blocks xs

    check (_, Nothing) = 0
    check (i, Just j) = i * j

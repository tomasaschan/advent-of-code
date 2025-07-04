module Dec20
  ( solve,
    countByGains,
  )
where

import Data.List (sortBy)
import Data.Map (Map, insert, insertWith, toList, (!?))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set, fromList, member)
import Helpers.Data.Grid (neighbors)

solve :: Int -> String -> (String, String)
solve threshold input = (a, b)
  where
    (track, start) = parse input
    measured = runWithoutCheats track start

    a = show . length . filter ((>= threshold) . snd) $ findCheats 2 measured
    b = show . length . filter ((>= threshold) . snd) $ findCheats 20 measured

type Pos = (Int, Int)

countByGains :: [(Pos, Int)] -> [(Int, Int)]
countByGains = sortBy (comparing fst) . toList . foldr (\(_, gain) acc -> insertWith (+) gain 1 acc) mempty

findCheats :: Int -> Map Pos Int -> [(Pos, Int)]
findCheats t m = concatMap (uncurry findCheats') (toList m)
  where
    findCheats' (x, y) n = valids $ mapMaybe (reachable (x, y) n) (toList m)
    reachable (x, y) n ((x', y'), n') =
      let d = manhattan (x, y) (x', y')
       in if d <= t && n' > n + d then Just ((x', y'), n' - n - d) else Nothing

    manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

    valids = keepFirstOfConsecutive . sortBy (comparing snd)

    keepFirstOfConsecutive :: [(Pos, Int)] -> [(Pos, Int)]
    keepFirstOfConsecutive [] = []
    keepFirstOfConsecutive [x] = [x]
    keepFirstOfConsecutive ((p, n) : (p', n') : rest)
      | n + 1 == n' && manhattan p p' == 1 = keepFirstOfConsecutive ((p, n) : rest)
      | otherwise = (p, n) : keepFirstOfConsecutive ((p', n') : rest)

runWithoutCheats :: Set Pos -> Pos -> Map Pos Int
runWithoutCheats track start = go [(start, 0)] mempty
  where
    go [] m = m
    go ((pos, steps) : rest) m =
      case m !? pos of
        Just n | n <= steps -> go rest m
        _ -> go (rest ++ next pos steps) (insert pos steps m)

    next pos steps =
      [ (newPos, steps + 1)
        | newPos <- neighbors pos,
          newPos `member` track
      ]

parse :: String -> (Set Pos, Pos)
parse input = (track, start)
  where
    cells = [((x, y), c) | (y, line) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] line]

    track = fromList . fmap fst . filter ((`elem` ['.', 'S', 'E']) . snd) $ cells
    start = case fmap fst . filter ((== 'S') . snd) $ cells of
      [s] -> s
      _ -> error "There should be exactly one start position"

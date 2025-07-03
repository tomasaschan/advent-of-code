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

    cheats = findCheats measured

    a = show . length . filter ((>= threshold) . snd) $ cheats
    b = ""

type Pos = (Int, Int)

countByGains :: [(Pos, Int)] -> [(Int, Int)]
countByGains = sortBy (comparing fst) . toList . foldr (\(_, gain) acc -> insertWith (+) gain 1 acc) mempty

findCheats :: Map Pos Int -> [(Pos, Int)]
findCheats m = concatMap (uncurry (findCheats' m)) (toList m)
  where
    findCheats' m (x, y) n = mapMaybe (validate m ((x, y), n)) (tunnel (x, y))

    validate m (p, n) p' =
      case m !? p' of
        Just n' | n' > n + 2 -> Just (p, n' - n - 2)
        _ -> Nothing

    tunnel (x, y) = [(x + dx, y + dy) | dx <- [-2, 0, 2], dy <- [-2, 0, 2], (dx == 0) /= (dy == 0)]

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

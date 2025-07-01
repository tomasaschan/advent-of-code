module Dec18
  ( solve,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq (..), (<|), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe)
import Helpers.Data.Grid (neighbors)

solve :: Pos -> Int -> String -> (String, String)
solve bounds n = bimap a b . dupe . parse
  where
    a = maybe "<no path>" (show . subtract 1 . length) . bfs bounds . Set.fromList . take n
    b = maybe "<no cutoff>" (\(x, y) -> show x <> "," <> show y) . findCutoff n bounds

type Pos = (Int, Int)

bfs :: Pos -> Set Pos -> Maybe (NonEmpty Pos)
bfs bounds fallen = bfs' (((0, 0) :| []) <| Empty) mempty
  where
    bfs' :: Seq (NonEmpty Pos) -> Set Pos -> Maybe (NonEmpty Pos)
    bfs' Empty _ = Nothing
    bfs' q _ | Seq.length q > 1000 = Nothing
    bfs' ((p :| path) :<| _) _ | p == bounds = Just (p :| path)
    bfs' ((p :| _) :<| q) seen | p `member` seen = bfs' q seen
    bfs' (((p :| path) :<| q)) seen =
      let ps' = filter valid $ neighbors p
          q' = q >< Seq.fromList (fmap (:| (p : path)) ps')
          seen' = Set.insert p seen

          valid p' = all ($ p') validConditions

          validConditions =
            [ not . flip elem fallen,
              not . flip elem seen,
              (>= 0) . fst,
              (<= fst bounds) . fst,
              (>= 0) . snd,
              (<= snd bounds) . snd
            ]
       in bfs' q' seen'

findCutoff :: Int -> Pos -> [Pos] -> Maybe Pos
findCutoff n bounds bytes = findCutoff' (Set.fromList . take (n - 1) $ bytes) (drop (n - 1) bytes) Nothing
  where
    findCutoff' :: Set Pos -> [Pos] -> Maybe (Set Pos) -> Maybe Pos
    findCutoff' _ [] _ = Nothing
    findCutoff' fallen (p : falling) Nothing =
      case bfs bounds (Set.insert p fallen) of
        Just path -> findCutoff' (Set.insert p fallen) falling (Just (foldl (flip Set.insert) mempty path))
        Nothing -> Just p
    findCutoff' fallen (p : falling) (Just known)
      | p `Set.member` known = findCutoff' fallen (p : falling) Nothing
      | otherwise = findCutoff' (Set.insert p fallen) falling (Just known)

parse :: String -> [Pos]
parse = fmap readLine . lines
  where
    readLine :: String -> (Int, Int)
    readLine = bimap read read . parts ""

    parts prefix (',' : suffix) = (prefix, suffix)
    parts prefix (c : rest) = parts (prefix ++ [c]) rest
    parts prefix [] = (prefix, "")

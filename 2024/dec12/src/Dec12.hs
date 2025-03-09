module Dec12
  ( solve,
  )
where

import Data.Bifunctor (bimap)
import Data.Map (member, (!), (!?))
import Data.Sequence (Seq (..), empty, fromList, singleton, (><))
import Data.Set (Set, insert)
import Data.Tuple.Extra (dupe)
import Helpers.Data.Grid (Grid, World (..), neighbors, parse)

solve :: String -> (String, String)
solve = bimap a b . dupe
  where
    a = show . flood' . parse (,) Just
    b = const ""

flood' :: Grid Char -> Int
flood' g = flood g mempty (singleton (0, 0)) empty 0 (0, 0)

-- todo: parse to a grid, floodfill each region, counting size and perimiter along the way
flood :: Grid Char -> Set (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int) -> Int -> (Int, Int) -> Int
flood _ _ Empty Empty subtotal (area, perimeter) = subtotal + area * perimeter
flood g seen Empty (next :<| rest) subtotal (area, perimeter) =
  if next `elem` seen
    then flood g seen Empty rest (subtotal + area * perimeter) (0, 0)
    else flood g seen (singleton next) rest (subtotal + area * perimeter) (0, 0)
flood (W g) seen (current :<| rest) next subtotal (area, perimeter) =
  if current `elem` seen
    then flood (W g) seen rest next subtotal (area, perimeter)
    else
      let seen' = insert current seen
          steps = neighbors current
          outside = filter ((/= Just (g ! current)) . (g !?)) steps
          within = filter ((== Just (g ! current)) . (g !?)) steps
          perimeter' = perimeter + length outside
          area' = area + 1
          rest' = rest >< fromList (filter (`notElem` seen') within)
          next' = next >< fromList (filter (`notElem` seen') . filter (`member` g) $ outside)
       in flood (W g) seen' rest' next' subtotal (area', perimeter')

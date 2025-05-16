module Dec06
  ( solve,
    endsInLoop,
    countLoops,
    parseInput,
  )
where

import Data.Map.Strict as Map
import Data.Set as Set
import Helpers.Data.Grid

solve :: String -> (String, String)
solve input = (a, b)
  where
    (m, g) = parseInput input
    a = show $ length $ traceToOutOfBounds m g
    b = show $ countLoops m mempty g

parseInput :: String -> (Grid Bool, Guard)
parseInput input = (m, g)
  where
    m = parse (,) space input
    W gp = parse (,) guard input
    g = uncurry (flip Guard) $ head $ Map.toList gp

space :: Char -> Maybe Bool
space '#' = Just True
space _ = Just False

type Pos = (Int, Int)

type Lab = Grid Bool

data Dir = U | L | D | R deriving (Show, Eq, Ord)

data Guard = Guard Dir Pos deriving (Show, Eq, Ord)

pos :: Guard -> Pos
pos (Guard _ p) = p

guard :: Char -> Maybe Dir
guard '^' = Just U
guard '<' = Just L
guard 'v' = Just D
guard '>' = Just R
guard _ = Nothing

from :: Dir -> Pos -> Pos
U `from` (x, y) = (x, y - 1)
L `from` (x, y) = (x - 1, y)
D `from` (x, y) = (x, y + 1)
R `from` (x, y) = (x + 1, y)

clockwise :: Dir -> Dir
clockwise U = R
clockwise R = D
clockwise D = L
clockwise L = U

traceToOutOfBounds :: Lab -> Guard -> Set Pos
traceToOutOfBounds m g = fst $ trace m pos False (Set.singleton (pos g)) g

endsInLoop :: Lab -> Guard -> Bool
endsInLoop m g = snd $ trace m id True mempty g

step :: Lab -> Guard -> Maybe Guard
step (W m) g =
  let (Guard d p) = g
      p' = d `from` p
      next False = Guard d p'
      next True = Guard (clockwise d) p
   in next <$> Map.lookup p' m

trace :: (Ord a) => Lab -> (Guard -> a) -> Bool -> Set a -> Guard -> (Set a, Bool)
trace _ f True seen g | f g `Set.member` seen = (seen, True)
trace (W m) f exitIfSeen seen g =
  let seen' = Set.insert (f g) seen
      (Guard d p) = g
   in case Map.lookup (d `from` p) m of
        Nothing -> (seen', False)
        Just False -> trace (W m) f exitIfSeen seen' (Guard d (d `from` p))
        Just True -> trace (W m) f exitIfSeen seen' (Guard (clockwise d) p)

countLoops :: Lab -> Set Pos -> Guard -> Int
countLoops (W m) tried g = loopsFromHere + loopsFromRest
  where
    inFrontOf (Guard d p) = d `from` p
    addInFront :: Lab
    addInFront = W $ Map.update (const $ Just True) (inFrontOf g) m

    tried' = Set.insert (inFrontOf g) tried

    loopsFromHere = if inFrontOf g `Set.notMember` tried && endsInLoop addInFront g then 1 else 0
    loopsFromRest = sum $ countLoops (W m) tried' <$> step (W m) g

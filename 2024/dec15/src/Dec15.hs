module Dec15
  ( solve,
  )
where

import Data.Foldable (maximumBy)
import qualified Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = show . coordinateSum . boxes . traceShowId $ advance $ parse input
    b = ""

type Pos = (Int, Int)

type Walls = Set Pos

type Boxes = Set Pos

type Robot = Pos

(⊕) :: Pos -> Pos -> Pos
(⊕) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(⊖) :: Pos -> Pos -> Pos
(⊖) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

data Direction = U | D | L | R deriving (Eq)

instance Show Direction where
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"

instance Read Direction where
  readsPrec _ ('^' : rest) = [(U, rest)]
  readsPrec _ ('v' : rest) = [(D, rest)]
  readsPrec _ ('<' : rest) = [(L, rest)]
  readsPrec _ ('>' : rest) = [(R, rest)]
  readsPrec _ _ = []

type Instructions = [Direction]

data State = State
  { walls :: Walls,
    boxes :: Boxes,
    robot :: Robot,
    instructions :: Instructions
  }

instance Show State where
  show State {walls = ws, boxes = bs, robot = r, instructions = instrs} =
    concatMap show instrs ++ "\n" ++ unlines rows
    where
      rows = [row y | y <- [0 .. maxY]]
      row y = [at x y | x <- [0 .. maxX]]

      at x y
        | (x, y) == r = '@'
        | (x, y) `Set.member` bs = 'O'
        | (x, y) `Set.member` ws = '#'
        | otherwise = ' '

      maxX = fst $ maximumBy (comparing fst) ws
      maxY = snd $ maximumBy (comparing snd) ws

advance :: State -> State
advance state@State {instructions = []} = state
advance
  state@State
    { robot = r,
      instructions = (i : rest),
      walls = ws,
      boxes = bs
    } = advance next'
    where
      dp = case i of
        U -> (0, -1)
        D -> (0, 1)
        L -> (-1, 0)
        R -> (1, 0)

      next' = maybe (state {instructions = rest}) (shift state) (target r)

      target :: Pos -> Maybe Pos
      target p | p == r = target (p ⊕ dp)
      target p | p `Set.member` ws = Nothing
      target p | p `Set.member` bs = target (p ⊕ dp)
      target p = Just p

      shift :: State -> Pos -> State
      shift s' p | p' == r = s' {robot = p, instructions = rest}
        where
          p' = p ⊖ dp
      shift s' p | p' `Set.member` bs = shift s'' p'
        where
          p' = p ⊖ dp
          bx' = Set.insert p $ Set.delete p' (boxes s')
          s'' = s' {boxes = bx'}
      shift s' p = error $ "invalid move " ++ show i ++ "@" ++ show p ++ "; cannot shift " ++ show (p ⊖ dp) ++ " to " ++ show p ++ "\n\n" ++ show s'

coordinateSum :: Boxes -> Int
coordinateSum = sum . map (\(x, y) -> x + 100 * y) . Set.toList

parse :: String -> State
parse input = State {walls = ws, boxes = bs, robot = r, instructions = instrs}
  where
    (map', instructions') = case splitOn "\n\n" input of
      [m', i'] -> (m', filter (/= '\n') i')
      _ -> error "Invalid input format"

    ws = findAll '#' map'
    bs = findAll 'O' map'
    r = Set.findMin $ findAll '@' map'

    findAll c str = Set.fromList $ concatMap (\(j, line) -> [(i, j) | (i, x) <- zip [0 ..] line, x == c]) $ zip [0 ..] (lines str)

    instrs = read . Data.List.singleton <$> instructions'

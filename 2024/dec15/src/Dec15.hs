module Dec15
  ( solve,
    State (..),
    advance,
    next',
    done,
    coordinateSum,
    parse,
  )
where

import Data.Foldable (maximumBy)
import qualified Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = show . coordinateSum . boxes . advance $ parse input
    b = ""

type Pos = (Int, Int)

type Walls = Set Pos

type Boxes = Set Pos

type Robot = Pos

data Direction = U | D | L | R deriving (Eq)

m :: Direction -> Pos -> Pos
m U (x, y) = (x, y - 1)
m D (x, y) = (x, y + 1)
m L (x, y) = (x - 1, y)
m R (x, y) = (x + 1, y)

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
    unlines rows ++ "\n\n\n" ++ concatMap show instrs ++ "\n\n" ++ show (coordinateSum bs)
    where
      rows = [row y | y <- [0 .. maxY]]
      row y = [at x y | x <- [0 .. maxX]]

      i = case instrs of
        (U : _) -> '^'
        (D : _) -> 'v'
        (L : _) -> '<'
        (R : _) -> '>'
        [] -> '@'

      at x y
        | (x, y) == r = i
        | (x, y) `Set.member` bs = 'O'
        | (x, y) `Set.member` ws = '#'
        | otherwise = ' '

      maxX = fst $ maximumBy (comparing fst) ws
      maxY = snd $ maximumBy (comparing snd) ws

done :: State -> Bool
done State {instructions = []} = True
done _ = False

advance :: State -> State
advance state@State {instructions = []} = state
advance state = advance (next' state)

next' :: State -> State
next' state@State {instructions = []} = state
next' state@State {instructions = (i : rest)} = maybe (state {instructions = rest}) shift candidates
  where
    candidates = boxesToMove state
    shift bs =
      let shifted = Set.map (m i) bs
          bs' = Set.union shifted (Set.difference (boxes state) bs)
       in state {instructions = rest, robot = m i (robot state), boxes = bs'}

boxesToMove :: State -> Maybe Boxes
boxesToMove State {instructions = []} = Nothing
boxesToMove State {robot = r, walls = ws, boxes = bs, instructions = (i : _)} = boxesToMove' (Seq.singleton (m i r)) Set.empty
  where
    boxesToMove' :: Seq Pos -> Boxes -> Maybe Boxes
    boxesToMove' Seq.Empty cs = Just cs
    boxesToMove' (next :<| _) _ | next `Set.member` ws = Nothing
    boxesToMove' (b :<| q) cs | b `Set.member` bs = boxesToMove' (q :|> m i b) (Set.insert b cs)
    boxesToMove' (_ :<| q) cs = boxesToMove' q cs

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

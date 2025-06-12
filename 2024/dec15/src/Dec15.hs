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

import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (maximumBy)
import qualified Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Sequence (Seq ((:<|), (:|>)), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = solve' Normal
    b = solve' Wide

    solve' w = show . coordinateSum . boxes . advance $ parse w input

type Pos = (Int, Int)

type Walls = Set Pos

type Boxes = Set Pos

type Robot = Pos

data Direction = U | D | L | R deriving (Eq)

data Width = Normal | Wide deriving (Eq, Show, Read)

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
  { width :: Width,
    walls :: Walls,
    boxes :: Boxes,
    robot :: Robot,
    instructions :: Instructions
  }

instance Show State where
  show State {width = w, walls = ws, boxes = bs, robot = r, instructions = instrs} =
    unlines rows ++ "\n\n\n" ++ concatMap show instrs ++ "\n\n" ++ show (coordinateSum bs)
    where
      rows = [row y | y <- [0 .. maxY]]
      row y = concat [at x y | x <- [0 .. maxX]]

      i = case instrs of
        (U : _) -> "^"
        (D : _) -> "v"
        (L : _) -> "<"
        (R : _) -> ">"
        [] -> "@"

      at x y
        | (x, y) == r = i
        | w == Normal && (x, y) `Set.member` bs = "O"
        | w == Wide && (x, y) `Set.member` bs = "["
        | w == Wide && (x - 1, y) `Set.member` bs = "]"
        | (x, y) `Set.member` ws = "#"
        | otherwise = " "

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
boxesToMove State {width = Normal, robot = r, walls = ws, boxes = bs, instructions = (i : _)} = boxesToMove' (Seq.singleton (m i r)) Set.empty
  where
    boxesToMove' :: Seq Pos -> Boxes -> Maybe Boxes
    boxesToMove' Seq.Empty cs = Just cs
    boxesToMove' (next :<| _) _ | next `Set.member` ws = Nothing
    boxesToMove' (b :<| q) cs | b `Set.member` bs = boxesToMove' (q :|> m i b) (Set.insert b cs)
    boxesToMove' (_ :<| q) cs = boxesToMove' q cs
boxesToMove State {width = Wide, robot = r, walls = ws, boxes = bs, instructions = (i : _)} = boxesToMove' (Seq.singleton (m i r)) Set.empty
  where
    boxesToMove' :: Seq Pos -> Boxes -> Maybe Boxes
    boxesToMove' Seq.Empty cs = Just cs
    boxesToMove' (b :<| _) _ | isWall b = Nothing
    boxesToMove' (b :<| q) cs | isBox i b = boxesToMove' (q >< nextBox i b) (Set.insert (theBox i b) cs)
    boxesToMove' (_ :<| q) cs = boxesToMove' q cs

    isWall :: Pos -> Bool
    isWall p = p `Set.member` ws

    isBox :: Direction -> Pos -> Bool
    isBox U b = b `Set.member` bs || m L b `Set.member` bs
    isBox D b = b `Set.member` bs || m L b `Set.member` bs
    isBox L b = m L b `Set.member` bs
    isBox R b = b `Set.member` bs

    nextBox :: Direction -> Pos -> Seq Pos
    nextBox U b
      | b `Set.member` bs = Seq.fromList [m U b, m R (m U b)]
      | m L b `Set.member` bs = Seq.fromList [m L (m U b), m U b]
      | otherwise = Seq.empty
    nextBox D b
      | b `Set.member` bs = Seq.fromList [m D b, m R (m D b)]
      | m L b `Set.member` bs = Seq.fromList [m L (m D b), m D b]
      | otherwise = Seq.empty
    nextBox L b
      | m L b `Set.member` bs = Seq.fromList [m L (m L b)]
      | otherwise = Seq.empty
    nextBox R b
      | b `Set.member` bs = Seq.fromList [m R (m R b)]
      | otherwise = Seq.empty

    theBox :: Direction -> Pos -> Pos
    theBox U b
      | b `Set.member` bs = b
      | m L b `Set.member` bs = m L b
      | otherwise = error "Invalid box position for U"
    theBox D b
      | b `Set.member` bs = b
      | m L b `Set.member` bs = m L b
      | otherwise = error "Invalid box position for D"
    theBox L b
      | m L b `Set.member` bs = m L b
      | otherwise = error "Invalid box position for L"
    theBox R b
      | b `Set.member` bs = b
      | otherwise = error "Invalid box position for R"

coordinateSum :: Boxes -> Int
coordinateSum = sum . map (\(x, y) -> x + 100 * y) . Set.toList

parse :: Width -> String -> State
parse w input = State {width = w, walls = ws, boxes = bs, robot = r, instructions = instrs}
  where
    (map', instructions') = case splitOn "\n\n" input of
      [m', i'] -> (m', filter (/= '\n') i')
      _ -> error "Invalid input format"

    ws =
      let ws' = findAll '#' map'
       in case w of
            Normal -> ws'
            Wide -> Set.union ws' (Set.map (first (+ 1)) ws')

    bs = findAll 'O' map'
    r = Set.findMin $ findAll '@' map'
    w' = case w of
      Normal -> 1
      Wide -> 2
    findAll c str = Set.fromList $ concatMap (\(j, line) -> [(i * w', j) | (i, x) <- zip [0 ..] line, x == c]) $ zip [0 ..] (lines str)

    instrs = read . Data.List.singleton <$> instructions'

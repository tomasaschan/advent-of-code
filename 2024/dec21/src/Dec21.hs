module Dec21
  ( solve,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import qualified Data.Map as Map (insert, lookup)
import Data.Map.Strict (Map)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = show . sum . fmap (complexity 2) $ parse input
    b = show . sum . fmap (complexity 25) $ parse input

type Cache = Map (Int, [Directional]) Int

complexity :: Int -> [Numeric] -> Int
complexity robots code =
  let steps = dial robots code
      numericPart = read . pretty . filter (/= NA) $ code :: Int
   in steps * numericPart

dial :: Int -> [Numeric] -> Int
dial robots = flip evalState mempty . dial' robots . parts . dialN
  where
    dial' :: Int -> [[Directional]] -> State Cache Int
    dial' n _ | n < 0 || n > robots = error "infinite recursion prevented"
    dial' 1 code = dial'' 1 code $ return . length
    dial' n code = dial'' n code $ dial' (n - 1) . parts

    dial'' :: Int -> [[Directional]] -> ([Directional] -> State Cache Int) -> State Cache Int
    dial'' _ [] _ = return 0
    dial'' n (part : rest) f = do
      cached <- gets (Map.lookup (n, part))
      rest' <- dial'' n rest f
      case cached of
        Just steps -> return $ steps + rest'
        Nothing -> do
          let ps = dialD part
          next <- f ps
          modify (Map.insert (n, part) next)
          return $ next + rest'

dialN :: [Numeric] -> [Directional]
dialN = dialN' NA
  where
    dialN' _ [] = []
    dialN' current (c : code) = gon current c ++ (DA : dialN' c code)

dialD :: [Directional] -> [Directional]
dialD = dialD' DA
  where
    dialD' _ [] = []
    dialD' current (c : code) = god current c ++ (DA : dialD' c code)

parts :: [Directional] -> [[Directional]]
parts = parts' [] []
  where
    parts' all' acc [] = reverse $ acc : all'
    parts' all' acc (DA : code) = parts' (reverse (DA : acc) : all') [] code
    parts' all' acc (c : code) = parts' all' (c : acc) code

data Directional = L | R | U | D | DA
  deriving (Eq, Ord)

instance Show Directional where
  show L = "<"
  show R = ">"
  show U = "^"
  show D = "v"
  show DA = "A"

pretty :: (Show c) => [c] -> String
pretty = concatMap show

god :: Directional -> Directional -> [Directional]
god U U = []
god U DA = [R]
god U L = [D, L]
god U D = [D]
god U R = [D, R]
god L U = [R, U]
god L DA = [R, R, U]
god L L = []
god L D = [R]
god L R = [R, R]
god D U = [U]
god D DA = [U, R]
god D L = [L]
god D D = []
god D R = [R]
god R U = [L, U]
god R DA = [U]
god R L = [L, L]
god R D = [L]
god R R = []
god DA U = [L]
god DA DA = []
god DA L = [D, L, L]
god DA D = [L, D]
god DA R = [D]

data Numeric = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | NA
  deriving (Eq, Enum, Ord)

instance Show Numeric where
  show NA = "A"
  show n = show . fromEnum $ n

gon :: Numeric -> Numeric -> [Directional]
gon NA NA = []
gon NA Zero = [L]
gon NA One = [U, L, L]
gon NA Two = [L, U]
gon NA Three = [U]
gon NA Four = [U, U, L, L]
gon NA Five = [L, U, U]
gon NA Six = [U, U]
gon NA Seven = [U, U, U, L, L]
gon NA Eight = [L, U, U, U]
gon NA Nine = [U, U, U]
gon Zero NA = [R]
gon Zero Zero = []
gon Zero One = [U, L]
gon Zero Two = [U]
gon Zero Three = [U, R]
gon Zero Four = [U, U, L]
gon Zero Five = [U, U]
gon Zero Six = [U, U, R]
gon Zero Seven = [U, U, U, L]
gon Zero Eight = [U, U, U]
gon Zero Nine = [U, U, U, R]
gon One NA = [R, R, D]
gon One Zero = [R, D]
gon One One = []
gon One Two = [R]
gon One Three = [R, R]
gon One Four = [U]
gon One Five = [U, R]
gon One Six = [U, R, R]
gon One Seven = [U, U]
gon One Eight = [U, U, R]
gon One Nine = [U, U, R, R]
gon Two NA = [D, R]
gon Two Zero = [D]
gon Two One = [L]
gon Two Two = []
gon Two Three = [R]
gon Two Four = [L, U]
gon Two Five = [U]
gon Two Six = [U, R]
gon Two Seven = [L, U, U]
gon Two Eight = [U, U]
gon Two Nine = [U, U, R]
gon Three NA = [D]
gon Three Zero = [D, L]
gon Three One = [L, L]
gon Three Two = [L]
gon Three Three = []
gon Three Four = [L, L, U]
gon Three Five = [L, U]
gon Three Six = [U]
gon Three Seven = [L, L, U, U]
gon Three Eight = [L, U, U]
gon Three Nine = [U, U]
gon Four NA = [R, R, D, D]
gon Four Zero = [R, D, D]
gon Four One = [D]
gon Four Two = [R, D]
gon Four Three = [R, R, D]
gon Four Four = []
gon Four Five = [R]
gon Four Six = [R, R]
gon Four Seven = [U]
gon Four Eight = [U, R]
gon Four Nine = [U, R, R]
gon Five NA = [D, D, R]
gon Five Zero = [D, D]
gon Five One = [L, D]
gon Five Two = [D]
gon Five Three = [D, R]
gon Five Four = [L]
gon Five Five = []
gon Five Six = [R]
gon Five Seven = [L, U]
gon Five Eight = [U]
gon Five Nine = [U, R]
gon Six NA = [D, D]
gon Six Zero = [D, D, L]
gon Six One = [D, L, L]
gon Six Two = [D, L]
gon Six Three = [D]
gon Six Four = [L, L]
gon Six Five = [L]
gon Six Six = []
gon Six Seven = [L, L, U]
gon Six Eight = [L, U]
gon Six Nine = [U]
gon Seven NA = [R, R, D, D, D]
gon Seven Zero = [R, D, D, D]
gon Seven One = [D, D]
gon Seven Two = [D, D, R]
gon Seven Three = [D, D, R, R]
gon Seven Four = [D]
gon Seven Five = [D, R]
gon Seven Six = [D, R, R]
gon Seven Seven = []
gon Seven Eight = [R]
gon Seven Nine = [R, R]
gon Eight NA = [D, D, D, R]
gon Eight Zero = [D, D, D]
gon Eight One = [D, D, L]
gon Eight Two = [D, D]
gon Eight Three = [D, D, R]
gon Eight Four = [D, L]
gon Eight Five = [D]
gon Eight Six = [D, R]
gon Eight Seven = [L]
gon Eight Eight = []
gon Eight Nine = [R]
gon Nine NA = [D, D, D]
gon Nine Zero = [L, D, D, D]
gon Nine One = [L, L, D, D]
gon Nine Two = [L, D, D]
gon Nine Three = [D, D]
gon Nine Four = [L, L, D]
gon Nine Five = [L, D]
gon Nine Six = [D]
gon Nine Seven = [L, L]
gon Nine Eight = [L]
gon Nine Nine = []

parse :: String -> [[Numeric]]
parse = fmap line . lines
  where
    line :: String -> [Numeric]
    line = fmap c
    c :: Char -> Numeric
    c '0' = Zero
    c '1' = One
    c '2' = Two
    c '3' = Three
    c '4' = Four
    c '5' = Five
    c '6' = Six
    c '7' = Seven
    c '8' = Eight
    c '9' = Nine
    c 'A' = NA
    c _ = error "Invalid character in input"

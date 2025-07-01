module Computer
  ( State (..),
    Program,
    Int3 (..),
    StepResult (..),
    newState,
    newProgram,
    step,
    halted,
    code,
  )
where

import Data.Array (Array, array, elems, (!))
import Data.Bits (xor)

data Int3 = Zero | One | Two | Three | Four | Five | Six | Seven deriving (Show, Eq, Ord, Enum)

type Program = Array Int Int3

code :: Program -> [Int]
code = fmap fromEnum . elems

data State = State {a :: Int, b :: Int, c :: Int, p :: Int, program :: Program} deriving (Show)

newProgram :: [Int3] -> Program
newProgram prog = array (0, length prog - 1) (zip [0 ..] prog)

newState :: Program -> Int -> Int -> Int -> State
newState prog a' b' c' =
  State
    { a = a',
      b = b',
      c = c',
      p = 0,
      program = prog
    }

type Operand = Int3

combo :: State -> Operand -> Int
combo _ Zero = 0
combo _ One = 1
combo _ Two = 2
combo _ Three = 3
combo s Four = a s
combo s Five = b s
combo s Six = c s
combo _ Seven = error "use of reserved operand 7 in combo operator"

halted :: State -> Bool
halted s = p s < 0 || p s >= length (program s) - 1

data StepResult = NewState State | Output State Int

step :: State -> StepResult
step s =
  let opr = program s ! (1 + p s)
   in case program s ! p s of
        Zero -> NewState $ adv s opr
        One -> NewState $ bxl s opr
        Two -> NewState $ bst s opr
        Three -> NewState $ jnz s opr
        Four -> NewState $ bxc s opr
        Five -> uncurry Output $ out s opr
        Six -> NewState $ bdv s opr
        Seven -> NewState $ cdv s opr

adv :: State -> Operand -> State
adv s op = s'
  where
    numerator = a s
    denominator = 2 ^ combo s op
    a' = numerator `div` denominator
    s' = s {a = a', p = p s + 2}

bxl :: State -> Operand -> State
bxl s op = s'
  where
    b' = b s `xor` fromEnum op
    s' = s {b = b', p = p s + 2}

bst :: State -> Operand -> State
bst s op = s'
  where
    b' = combo s op `mod` 8
    s' = s {b = b', p = p s + 2}

jnz :: State -> Operand -> State
jnz s op = s'
  where
    s' =
      if a s == 0
        then s {p = p s + 2}
        else s {p = fromEnum op}

bxc :: State -> Operand -> State
bxc s _ = s'
  where
    b' = b s `xor` c s
    s' = s {b = b', p = p s + 2}

out :: State -> Operand -> (State, Int)
out s op = (s', output')
  where
    output' = combo s op `mod` 8
    s' = s {p = p s + 2}

bdv :: State -> Operand -> State
bdv s op = s'
  where
    numerator = a s
    denominator = 2 ^ combo s op
    b' = numerator `div` denominator
    s' = s {b = b', p = p s + 2}

cdv :: State -> Operand -> State
cdv s op = s'
  where
    numerator = a s
    denominator = 2 ^ combo s op
    c' = numerator `div` denominator
    s' = s {c = c', p = p s + 2}

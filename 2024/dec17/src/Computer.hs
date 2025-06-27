module Computer
  ( step,
    State (..),
    output,
    Int3 (..),
    initialize,
    run,
  )
where

import Data.Array (Array, array, (!))
import Data.Bits (xor)
import Data.List (intercalate)

data Int3 = Zero | One | Two | Three | Four | Five | Six | Seven deriving (Show, Eq, Ord, Enum)

data State = State {a :: Int, b :: Int, c :: Int, p :: Int, program :: Array Int Int3, _output :: [Int]} deriving (Show)

output :: State -> String
output = intercalate "," . fmap show . reverse . _output

initialize :: [Int3] -> Int -> Int -> Int -> State
initialize prog a' b' c' =
  State
    { a = a',
      b = b',
      c = c',
      p = 0,
      program = array (0, length prog - 1) (zip [0 ..] prog),
      _output = []
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

run :: State -> State
run s | p s < 0 = s
run s | p s >= length (program s) - 1 = s
run s = run $ step s

step :: State -> State
step s =
  let opr = program s ! (1 + p s)
      op = case program s ! p s of
        Zero -> adv
        One -> bxl
        Two -> bst
        Three -> jnz
        Four -> bxc
        Five -> out
        Six -> bdv
        Seven -> cdv
   in op s opr

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

out :: State -> Operand -> State
out s op = s'
  where
    output' = (combo s op `mod` 8) : _output s
    s' = s {_output = output', p = p s + 2}

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

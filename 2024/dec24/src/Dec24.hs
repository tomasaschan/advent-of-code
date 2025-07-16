module Dec24
  ( solve,
    visualizeCircuit,
    toDot,
  )
where

import Control.Monad.State (State, gets, modify, runState)
import Data.Bits (bit, xor)
import Data.Graph.DGraph (DGraph, fromArcsList)
import Data.Graph.Types (Arc, (-->))
import Data.Graph.Visualize (plotDGraphPng)
import Data.List (sort, stripPrefix)
import Data.Map (Map, fromList, insert, keys, toList, (!))
import Data.Maybe (isJust)
import Text.ParserCombinators.Parsec hiding (State)

solve :: String -> (String, String)
solve input = (either show a c', either show b c')
  where
    c' = parse' input

    a c =
      let wires = outputWires c
          outputs = values wires c
       in show $ interpret outputs

    --- what nodes to swap was here determined by rendering the circuit in dot and inspecting each adder
    b = toDot . swapBack "z11" "wpd" . swapBack "skh" "jqf" . swapBack "z19" "mdd" . swapBack "wts" "z37"

type Wire = String

data Input = Value Bool | And Wire Wire | Or Wire Wire | Xor Wire Wire deriving (Show, Eq)

type Circuit = Map Wire Input

interpret :: [Bool] -> Int
interpret = sum . fmap f . zip [0 ..]
  where
    f (_, False) = 0
    f (i, True) = bit i

outputWires :: Circuit -> [Wire]
outputWires = filter (isJust . stripPrefix "z") . keys

values :: [Wire] -> Circuit -> [Bool]
values [] _ = []
values (w : ws) c =
  let (v, c') = runState (evaluate w) c
   in v : values ws c'

evaluate :: Wire -> State Circuit Bool
evaluate w = do
  current <- gets (! w)

  case current of
    Value v -> return v
    And a b -> binary (&&) a b
    Or a b -> binary (||) a b
    Xor a b -> binary xor a b
  where
    binary f a b = do
      a' <- evaluate a
      b' <- evaluate b
      let v = f a' b'

      modify (insert w (Value v))

      return v

visualizeCircuit :: String -> IO ()
visualizeCircuit input = do
  case parse' input of
    Left e -> print e
    Right c -> do
      let g = toGraph c
      fp <- plotDGraphPng g "graph.png"
      print fp

swapBack :: Wire -> Wire -> Circuit -> Circuit
swapBack a b c =
  let va = c ! a
      vb = c ! b
   in insert a vb . insert b va $ c

-- runAndWait :: IO ThreadId -> IO ()
-- runAndWait io = do
--   i <- io
--   print i
--   s <- threadStatus i
--   print s
--   threadDelay $ 1000 * 1000 * 5
--   s' <- threadStatus i
--   print s'

toDot :: Circuit -> String
toDot c = unlines (preamble ++ sort (nodes $ toList c) ++ sort (edges $ toList c) ++ postscript)
  where
    preamble = ["digraph G {", "rankdir=\"LR\";", "rank=\"source\";"]
    postscript = ["}", ""]

    nodes :: [(Wire, Input)] -> [String]
    nodes [] = []
    nodes (('x' : n, _) : xs) = ('x' : n) : nodes xs
    nodes (('y' : n, _) : xs) = ('y' : n) : nodes xs
    nodes (('z' : n, _) : xs) = ('z' : n) : nodes xs
    nodes (_ : xs) = nodes xs

    edges :: [(Wire, Input)] -> [String]
    edges [] = []
    edges (x : xs) = edges' x ++ edges xs

    edges' (_, Value _) = []
    edges' (w, And a b) = [a ++ " -> " ++ w ++ " [fillcolor=green]", b ++ " -> " ++ w ++ " [fillcolor=green]"]
    edges' (w, Or a b) = [a ++ " -> " ++ w ++ " [fillcolor=yellow]", b ++ " -> " ++ w ++ " [fillcolor=yellow]"]
    edges' (w, Xor a b) = [a ++ " -> " ++ w ++ " [fillcolor=red]", b ++ " -> " ++ w ++ " [fillcolor=red]"]

toGraph :: Circuit -> DGraph String ()
toGraph = fromArcsList . toArcs 0 . toList
  where
    toArcs :: Int -> [(Wire, Input)] -> [Arc String ()]
    toArcs _ [] = []
    toArcs n (x : xs) =
      let (n', as) = toArcs' n x
       in as ++ toArcs n' xs

    toArcs' n (_, Value _) = (n, [])
    toArcs' n (w, And a b) = (n + 1, [a --> gate, b --> gate, gate --> w]) where gate = "and:" ++ show n
    toArcs' n (w, Or a b) = (n + 1, [a --> gate, b --> gate, gate --> w]) where gate = "or:" ++ show n
    toArcs' n (w, Xor a b) = (n + 1, [a --> gate, b --> gate, gate --> w]) where gate = "xor:" ++ show n

parse' :: String -> Either ParseError Circuit
parse' = parse circuit "input"

circuit :: GenParser Char st Circuit
circuit = do
  vs <- sepEndBy1 value (string "\n") <* string "\n"

  ws <- sepEndBy1 wire (string "\n")

  return $ fromList (vs ++ ws)

value :: GenParser Char st (Wire, Input)
value = do
  w <- many1 alphaNum
  _ <- string ": "
  v <- string "0" <|> string "1"

  return (w, Value $ v == "1")

wire :: GenParser Char st (Wire, Input)
wire = do
  a <- many1 alphaNum
  _ <- string " "
  op <- string "AND" <|> string "OR" <|> string "XOR"
  _ <- string " "
  b <- many1 alphaNum
  _ <- string " -> "
  w <- many1 alphaNum

  case op of
    "AND" -> return (w, And a b)
    "OR" -> return (w, Or a b)
    "XOR" -> return (w, Xor a b)
    _ -> fail $ "Unknown operator: " ++ op

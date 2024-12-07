module Dec03 (
    solve,
    parse,
    Mul(..)
) where

import Debug.Trace
import Text.Regex

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = show . eval . parse True True $ input
    b = show . eval . parse False True $ input

data Mul = Mul Int Int deriving Eq

instance Show Mul where
    show (Mul a b) = show a <> "*" <> show b

eval :: [Mul] -> Int
eval = sum . map (\(Mul a b) -> a * b)

mulRx :: Regex
mulRx = mkRegex "mul\\(([0-9]+),([0-9]+)\\)"

parse :: Bool -> Bool -> String -> [Mul]
parse _ _ "" = []
parse False _ ('d':'o':'(':')':s ) = parse False True s
parse False _ ('d':'o':'n':'\'':'t':'(':')':s) = parse False False s
parse alwaysOn True ('m':'u':'l':'(':s) = case matchRegexAll (mkRegex "([0-9]+),([0-9]+)\\)") s of
    Just ("", _, t, [a, b]) -> Mul (read a) (read b) : parse alwaysOn True t
    _ -> parse alwaysOn True s
parse alwaysOn enabled (_:s) = parse alwaysOn enabled s

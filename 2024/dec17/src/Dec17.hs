module Dec17
  ( solve,
  )
where

import Computer
  ( Program,
    State,
    StepResult (..),
    code,
    halted,
    newProgram,
    newState,
    program,
    step,
  )
import Data.List (intercalate, isSuffixOf)
import Data.Set (insert, member)
import Text.ParserCombinators.Parsec hiding (State)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = either show (intercalate "," . fmap show . collectOutput) . parse' $ input
    b = either show (maybe "" show . findQuine . program) . parse' $ input

collectOutput :: State -> [Int]
collectOutput s = reverse $ collectOutput' s []
  where
    collectOutput' s' o | halted s' = o
    collectOutput' s' o =
      case step s' of
        NewState s'' -> collectOutput' s'' o
        Output s'' o' -> collectOutput' s'' (o' : o)

findQuine :: Program -> Maybe Int
findQuine prog = findQuine' (117440 : [0 .. 7]) mempty
  where
    expected = code prog

    findQuine' [] _ = Nothing
    findQuine' (a : as) seen | a `member` seen = findQuine' as (insert a seen)
    findQuine' (a : as) seen = result . collectOutput $ newState prog a 0 0
      where
        result o'
          | o' == expected = Just a
          | length o' >= length expected = findQuine' as (insert a seen)
          | o' `isSuffixOf` expected = findQuine' ([a * 8 + i | i <- [0 .. 7]] ++ as) (insert a seen)
          | otherwise = findQuine' as (insert a seen)

parse' :: String -> Either ParseError State
parse' = parse state' "input"

register' :: Char -> GenParser Char st Int
register' r = do
  _ <- string "Register "
  _ <- char r
  _ <- string ": "
  n <- many1 digit <* newline
  return $ read n

program' :: GenParser Char st Program
program' = do
  values <- many1 (choice [try (many1 digit <* char ','), many1 digit]) <* newline
  return $ newProgram $ fmap (toEnum . read) values

state' :: GenParser Char st State
state' = do
  a' <- register' 'A'
  b' <- register' 'B'
  c' <- register' 'C'
  _ <- newline

  prog <- string "Program: " *> program'

  return $ newState prog a' b' c'

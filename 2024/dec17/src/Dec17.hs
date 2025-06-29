module Dec17
  ( solve,
  )
where

import Computer
  ( Program,
    State,
    collectOutput,
    newProgram,
    newState,
  )
import Text.ParserCombinators.Parsec hiding (State)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = either show collectOutput . parse' $ input
    b = ""

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

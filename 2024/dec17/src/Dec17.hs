module Dec17
  ( solve,
  )
where

import Computer (Int3, State, initialize, output, run)
import Text.ParserCombinators.Parsec hiding (State)

solve :: String -> (String, String)
solve input = (a, b)
  where
    a = either show (output . run) . parse' $ input
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

program' :: GenParser Char st [Int3]
program' = do
  values <- many1 (choice [try (many1 digit <* char ','), many1 digit]) <* newline
  return $ fmap (toEnum . read) values

state' :: GenParser Char st State
state' = do
  a' <- register' 'A'
  b' <- register' 'B'
  c' <- register' 'C'
  _ <- newline

  prog <- string "Program: " *> program'

  return $ initialize prog a' b' c'

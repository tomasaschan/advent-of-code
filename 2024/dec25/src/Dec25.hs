module Dec25
  ( solve,
  )
where

import Data.Either (lefts, rights)
import Data.List (transpose)
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec

solve :: String -> String
solve = either show (show . uncurry countFits) . parse'

type Key = [Int]

type Lock = [Int]

type Item = Either Key Lock

countFits :: [Key] -> [Lock] -> Int
countFits keys locks = length [() | k <- keys, l <- locks, fits k l]

fits :: Key -> Lock -> Bool
fits k l = all (<= 7) $ zipWith (+) k l

parse' :: String -> Either ParseError ([Key], [Lock])
parse' = parse (both <* eof) "input"

both :: GenParser Char st ([Key], [Lock])
both = do
  items <- sepEndBy1 keyOrLock (string "\n")

  return (lefts items, rights items)

keyOrLock :: GenParser Char st Item
keyOrLock = try parseKey <|> parseLock

parseKey :: GenParser Char st Item
parseKey = do
  _ <- string "....." <* endOfLine
  parseItem Left 0

parseLock :: GenParser Char st Item
parseLock = do
  _ <- string "#####" <* endOfLine
  parseItem Right 1

line :: GenParser Char st [Bool]
line = do
  bits <- count 5 (char '.' <|> char '#') <* endOfLine
  return $ map (== '#') bits

parseItem :: ([Int] -> Item) -> Int -> GenParser Char st Item
parseItem f n = do
  ls <- count 6 line

  let ls' = map ((+ n) . length . filter id) $ transpose ls

  return $ f ls'

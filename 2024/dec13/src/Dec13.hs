module Dec13
  ( solve,
  )
where

import Data.Maybe (mapMaybe)
import GHC.Float (int2Double)
import Text.ParserCombinators.Parsec

solve :: String -> (String, String)
solve input = (a, b)
  where
    parsed = parse' input
    a = either show (solve' id) parsed
    b = either show (solve' bump) parsed

solve' :: (ClawMachine -> ClawMachine) -> [ClawMachine] -> String
solve' f = show . sum . fmap cost . mapMaybe (tryToWin . f)

data Button = Button {dx :: Int, dy :: Int} deriving (Eq)

instance Show Button where
  show b' = show (dx b', dy b')

data ClawMachine = ClawMachine
  { prize :: (Int, Int),
    ba :: Button,
    bb :: Button
  }

instance Show ClawMachine where
  show cm = "ClawMachine " ++ show (ba cm) ++ " " ++ show (bb cm) ++ " " ++ show (prize cm)

bump :: ClawMachine -> ClawMachine
bump cm = cm {prize = (px + 10000000000000, py + 10000000000000)}
  where
    (px, py) = prize cm

tryToWin :: ClawMachine -> Maybe (Int, Int)
tryToWin
  ClawMachine
    { prize = (px, py),
      ba = Button {dx = dxa, dy = dya},
      bb = Button {dx = dxb, dy = dyb}
    } =
    let det = int2Double $ dxa * dyb - dya * dxb
        a' = int2Double (dyb * px - dxb * py) / det
        b' = int2Double (dxa * py - dya * px) / det
        ai = round a' :: Int
        bi = round b' :: Int
     in if int2Double ai == a' && int2Double bi == b'
          then Just (ai, bi)
          else Nothing

cost :: (Int, Int) -> Int
cost (a, b) = 3 * a + b

parse' :: String -> Either ParseError [ClawMachine]
parse' = parse clawMachines "input"

clawMachines :: GenParser Char st [ClawMachine]
clawMachines = do
  machine <- clawMachine
  rest <- try (newline *> clawMachines) <|> return []
  return $ machine : rest

clawMachine :: GenParser Char st ClawMachine
clawMachine = do
  button_a <- parseButton 'A'
  button_b <- parseButton 'B'
  prize' <- parsePrize
  return $ ClawMachine prize' button_a button_b

parseButton :: Char -> GenParser Char st Button
parseButton l = do
  dx' <- string "Button " >> char l >> string ": X+" *> many1 digit
  dy' <- string ", Y+" *> many1 digit <* newline
  return $ Button (read dx') (read dy')

parsePrize :: GenParser Char st (Int, Int)
parsePrize = do
  x <- string "Prize: X=" *> many1 digit
  y <- string ", Y=" *> many1 digit <* newline
  return (read x, read y)

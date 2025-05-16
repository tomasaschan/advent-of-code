module Main (main) where

import Brick
import Dec15
import System.Environment
import qualified TUI (State (..), advance, done, draw, run)

newtype S = S {unS :: Dec15.State}

instance TUI.State S where
  advance = S . Dec15.advance . unS
  next = S . Dec15.next' . unS
  done = Dec15.done . unS
  draw s = [str . show . unS $ s]

main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [f] -> f
        _ -> error "Usage: dec15 <input file>"
  input <- readFile filename
  let initial = parse input
  final <- TUI.run (S initial)

  print (coordinateSum . boxes $ unS final)

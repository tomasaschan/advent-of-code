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

run :: [String] -> IO ()
-- no args; run non-interactive with input from stdin
run [] = do
  input <- getContents
  let (a, b) = Dec15.solve input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

-- one arg; run interactive with input from the indicated file
run [f] = do
  input <- readFile f
  let initial = parse input
  final <- TUI.run (S initial)
  print (coordinateSum . boxes $ unS final)

-- error: too many args
run _ = error "Usage: dec15 [<input-file>]\nIf no file is provided, input is read from stdin.\n\nerror: Too many arguments."

main :: IO ()
main = do
  args <- getArgs
  run args

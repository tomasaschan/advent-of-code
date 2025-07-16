module Main (main) where

import Dec25

main :: IO ()
main = do
  input <- getContents
  putStrLn $  solve input

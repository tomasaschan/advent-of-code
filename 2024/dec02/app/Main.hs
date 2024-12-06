module Main (main) where

import Dec02

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

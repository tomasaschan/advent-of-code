module Main (main) where

import Dec05

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b
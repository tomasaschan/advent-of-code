module Main (main) where

import Dec20

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve 100 input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

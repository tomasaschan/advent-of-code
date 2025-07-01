module Main (main) where

import Dec18

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve (70,70) 1024 input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

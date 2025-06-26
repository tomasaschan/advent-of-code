module Main (main) where

import Dec16

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

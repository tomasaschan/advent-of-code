module Main (main) where

import Dec22

main :: IO ()
main = do
  input <- getContents
  let (a,b) = solve input
  putStrLn $ "a: " <> a
  putStrLn $ "b: " <> b

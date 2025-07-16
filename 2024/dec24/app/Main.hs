module Main (main) where

import Data.List (intercalate, sort)
import Dec24
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.IOMode (IOMode (WriteMode))
import System.IO (hFlush, openFile)
import System.Process (callCommand)

main :: IO ()
main = do
  input <- getContents
  let (a, b) = solve input
  putStrLn $ "a: " <> a

  openFile "circuit.dot" WriteMode >>= \h -> do
    hPutStrLn h b
    hFlush h
    putStrLn "Circuit visualization written to circuit.dot"

  callCommand "dot -Tsvg circuit.dot -o circuit.svg"
  putStrLn "Circuit visualization written to circuit.svg"

  putStrLn . intercalate "," . sort $ ["z11", "wpd", "skh", "jqf", "z19", "mdd", "wts", "z37"]

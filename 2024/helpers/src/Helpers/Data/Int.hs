module Helpers.Data.Int (
  digits,
) where

import GHC.Float (int2Double)

digits :: Int -> Int
digits = (+ 1) . floor . logBase 10 . int2Double

module Test.Helpers
(
    describeInput
)
where

import Test.Hspec

describeInput :: String -> String -> (String -> a) -> SpecWith a -> Spec
describeInput description file solve =
  describe description
    . before (solve <$> readFile ("../../inputs/2024/" <> file))

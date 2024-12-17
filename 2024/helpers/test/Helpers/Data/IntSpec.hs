module Helpers.Data.IntSpec (spec) where

import Helpers.Data.Int
import Test.Hspec

spec :: Spec
spec = describe "digits" $ do
  it "1 has 1 digit" $ do
    digits 1 `shouldBe` 1
  it "12 has 2 digits" $ do
    digits 12 `shouldBe` 2
  it "123 has 3 digits" $ do
    digits 123 `shouldBe` 3
  it "1234 has 4 digits" $ do
    digits 1234 `shouldBe` 4

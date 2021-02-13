module CaveSpec where

import           Test.Hspec

import           Cave

spec :: Spec
spec = do
  describe "coordsTo" $ do
    it "returns in ascending order" $ do
      coordsTo (3, 4)
        `shouldBe` [ (0, 0)
                   , (0, 1)
                   , (0, 2)
                   , (0, 3)
                   , (0, 4)
                   , (1, 0)
                   , (1, 1)
                   , (1, 2)
                   , (1, 3)
                   , (1, 4)
                   , (2, 0)
                   , (2, 1)
                   , (2, 2)
                   , (2, 3)
                   , (2, 4)
                   , (3, 0)
                   , (3, 1)
                   , (3, 2)
                   , (3, 3)
                   , (3, 4)
                   ]

module Dec22Spec (spec) where

import Dec22
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 22" $ do
  describeInput "example" "22-example.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "37327623"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "23"

  describeInput "real input" "22.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "12664695565"

  describe "fundamental ops" $ do
    it "mix" $ do
      mix 42 15 `shouldBe` 37

    it "prune" $ do
      prune 100000000 `shouldBe` 16113920

    describe "evolve" $ do
      let examples =
            [ 123,
              15887950,
              16495136,
              527345,
              704524,
              1553684,
              12683156,
              11100544,
              12249484,
              7753432,
              5908254
            ]

      let pairs = zip examples (drop 1 examples)

      mapM_ (\(n, n') -> it ("evolve " ++ show n) $ evolve n `shouldBe` n') pairs

    describe "price" $ do
      it "price of 123" $ do
        price 123 `shouldBe` 3

      it "price of 15887950" $ do
        price 15887950 `shouldBe` 0

      it "price of 16495136" $ do
        price 16495136 `shouldBe` 6

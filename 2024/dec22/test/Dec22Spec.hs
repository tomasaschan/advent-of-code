module Dec22Spec (spec) where

import Dec22
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = describe "Dec 22" $ do
  describeInput "example a" "22-example-a.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "37327623"

  describeInput "example b" "22-example-b.txt" solve $ do
    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "23"

  describeInput "real input" "22.txt" solve $ do
    it "should solve a" $ \(a, _) -> do
      a `shouldBe` "12664695565"

    it "should solve b" $ \(_, b) -> do
      b `shouldBe` "1444"

  describe "fundamental ops" $ do
    it "prune" $ do
      prune 100000000 `shouldBe` 16113920

    it "evolve" $ do
      take 11 (iterate evolve 123)
        `shouldBe` [ 123,
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

    describe "price" $ do
      it "price of 123" $ do
        price 123 `shouldBe` 3

      it "price of 15887950" $ do
        price 15887950 `shouldBe` 0

      it "price of 16495136" $ do
        price 16495136 `shouldBe` 6

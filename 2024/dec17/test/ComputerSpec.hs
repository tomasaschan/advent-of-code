module ComputerSpec (spec) where

import Computer
import Test.Hspec

spec :: Spec
spec = describe "Computer" $ do
  context "example operations" $ do
    it "if register C contains 9, the program 2,6 would set register B to 1" $ do
      let s = initialize [Two, Six] 0 0 9
      let s' = run s
      b s' `shouldBe` 1

    it "if register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2" $ do
      let s = initialize [Five, Zero, Five, One, Five, Four] 10 0 0
      let s' = run s
      output s' `shouldBe` "0,1,2"

    describe "if register A contains 2024, the program 0,1,5,4,3,0" $ do
      let s = initialize [Zero, One, Five, Four, Three, Zero] 2024 0 0
      let s' = run s

      it "would output 4,2,5,6,7,7,7,7,3,1,0" $ do
        output s' `shouldBe` "4,2,5,6,7,7,7,7,3,1,0"
      it "would leave 0 in register A." $ do
        a s' `shouldBe` 0

    it "if register B contains 29, the program 1,7 would set register B to 26" $ do
      let s = initialize [One, Seven] 0 29 0
      let s' = run s
      b s' `shouldBe` 26

    it "if register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354" $ do
      let s = initialize [Four, Zero] 0 2024 43690
      let s' = run s
      b s' `shouldBe` 44354

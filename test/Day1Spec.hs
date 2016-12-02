module Main where

import Test.Hspec
import Day1 as Day1

spec :: Spec
spec = do
  describe "Day1" $ do
    context "parseStep" $ do
      it "should parse a left step" $ do
        Day1.parseStep "L1" `shouldBe` Day1.L 1

      it "should parse a right step" $ do
        Day1.parseStep "R100" `shouldBe` Day1.R 100

    context "parseInput" $ do
      it "should parse a list of steps" $ do
        Day1.parseInput "R1, L1, R2, R3, L2" `shouldBe` [Day1.R 1, Day1.L 1, Day1.R 2, Day1.R 3, Day1.L 2]

    context "getHqDist" $ do
      it "should return 5 for R2, L3" $ do
        Day1.getHqDist "R2, L3" `shouldBe` 5

      it "should return 2 for R2, R2, R2" $ do
        Day1.getHqDist "R2, R2, R2" `shouldBe` 2

      it "should return 12 for R5, L5, R5, R3" $ do
        Day1.getHqDist "R5, L5, R5, R3" `shouldBe` 12

      it "should return 239 for day1Input" $ do
        Day1.getHqDist day1Input `shouldBe` 239

main :: IO ()
main = hspec spec

day1Input = "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

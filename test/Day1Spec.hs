module Main where

import Test.Hspec
import Day1
import Data.Set as Set
import Data.Maybe

spec :: Spec
spec = do
  describe "Day1" $ do
    context "parseStep" $ do
      it "should parse a left step" $ do
        parseStep "L1" `shouldBe` L 1

      it "should parse a right step" $ do
        parseStep "R100" `shouldBe` R 100

    context "parseInput" $ do
      it "should parse a list of steps" $ do
        parseInput "R1, L1, R2, R3, L2" `shouldBe` [R 1, L 1, R 2, R 3, L 2]

    context "getHqDist" $ do
      it "should return 5 for R2, L3" $ do
        getHqDist (parseInput "R2, L3") `shouldBe` 5

      it "should return 2 for R2, R2, R2" $ do
        getHqDist (parseInput "R2, R2, R2") `shouldBe` 2

      it "should return 12 for R5, L5, R5, R3" $ do
        getHqDist (parseInput "R5, L5, R5, R3") `shouldBe` 12

      it "should return 239 for day1Input" $ do
        getHqDist (parseInput day1Input) `shouldBe` 239

    context "getPath" $ do
      it "should return correct path for R2, L3" $ do
        getPath [(N, (0,0))] [R 2, L 3] `shouldBe` [(N,(0,0)), (E,(1,0)), (E,(2,0)), (N,(2,1)), (N,(2,2)), (N,(2,3))]

      it "should return correct path for R8, R4, R4, R8" $ do
        getPath [(N, (0,0))] [R 8, R 4, R 4, R 8] `shouldBe` [(N,(0,0)),(E,(1,0)),(E,(2,0)),(E,(3,0)),(E,(4,0)),(E,(5,0)),(E,(6,0)),(E,(7,0)),(E,(8,0)),(S,(8,-1)),(S,(8,-2)),(S,(8,-3)),(S,(8,-4)),(W,(7,-4)),(W,(6,-4)),(W,(5,-4)),(W,(4,-4)),(N,(4,-3)),(N,(4,-2)),(N,(4,-1)),(N,(4,0)),(N,(4,1)),(N,(4,2)),(N,(4,3)),(N,(4,4))]

    context "getFirstCoordVisitedTwice" $ do
      it "should return Nothing for []" $ do
        getFirstCoordVisitedTwice [] Set.empty `shouldBe` Nothing

      it "should return Nothing for [(N, (0,0))]" $ do
        getFirstCoordVisitedTwice [(N, (0,0))] Set.empty `shouldBe` Nothing

      it "should return Just (0,0)" $ do
        getFirstCoordVisitedTwice [(N, (0,0)), (E, (1, 0)), (W, (0,0))] Set.empty `shouldBe` Just (0,0)

      it "should return Just (1,0)" $ do
        getFirstCoordVisitedTwice [(N, (0,0)), (E, (1, 0)), (E, (2,0)), (W, (1,0)), (W, (0,0))] Set.empty `shouldBe` Just (1,0)

      it "should return Just (4,0)" $ do
        getFirstCoordVisitedTwice (getPath [(N, (0,0))] [R 8, R 4, R 4, R 8]) Set.empty `shouldBe` Just (4,0)

      it "should return Just (-3,138) for day1Input" $ do
        getFirstCoordVisitedTwice (getPath [(N, (0,0))] $ parseInput day1Input) Set.empty `shouldBe` Just (-3,138)

      it "distance to first repeated coord should be 141 for day1Input" $ do
        let path = getPath [(N, (0,0))] $ parseInput day1Input
            maybeCoord = getFirstCoordVisitedTwice path Set.empty
         in (maybe 0 (getCoordDist) maybeCoord) `shouldBe` 141

main :: IO ()
main = hspec spec

day1Input = "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

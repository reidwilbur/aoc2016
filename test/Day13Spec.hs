module Main where

import Test.Hspec
import Day13
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Day13" $ do
    context "getNeighbors" $ do
      it "should return correct nbors" $ do
        getNeighbors 10 1 (1,1) Set.empty `shouldBe` [(1,(0,1)), (1,(1,2))]
        getNeighbors 10 1 (4,2) Set.empty `shouldBe` [(1,(3,2)), (1,(4,1))]
        getNeighbors 10 1 (6,5) Set.empty `shouldBe` [(1,(7,5)), (1,(5,5)), (1,(6,6)), (1,(6,4))]

    context "bfs" $ do
      it "should return correct number of steps for test input" $ do
        bfs 10 (7,4) Set.empty [(0, (1,1))] `shouldBe` 11

      it "should return correct number of steps" $ do
        bfs day13Input (31,39) Set.empty [(0, (1,1))] `shouldBe` 96

    context "bfs50" $ do
      it "should return correct number of positions" $ do
        Set.size (bfs50 day13Input Set.empty [(0, (1,1))]) `shouldBe` 141

main :: IO ()
main = hspec spec

day13Input = 1358

module Main where

import Test.Hspec
import Day12
import Data.Map as Map
import Data.List as List

spec :: Spec
spec = do
  describe "Day12" $ do
    context "parseInstr" $ do
      it "should parse cpy reg" $ do
        parseInstr "cpy a b" `shouldBe` CpReg {srcR=A, dst=B}

      it "should parse cpy imm" $ do
        parseInstr "cpy 34 b" `shouldBe` CpImm {srcI=34, dst=B}

      it "should parse inc" $ do
        parseInstr "inc d" `shouldBe` Inc D

      it "should parse dec" $ do
        parseInstr "dec a" `shouldBe` Dec A

      it "should parse jnz" $ do
        parseInstr "jnz c -2" `shouldBe` JnzReg {reg=C, ofs=(-2)}

      it "should parse jnz imm" $ do
        parseInstr "jnz 32 -2" `shouldBe` JnzImm {imm=32, ofs=(-2)}

    context "nextState" $ do
      it "should execute instr and return new state for cp imm" $ do
        let expstate = Map.fromList [(IP,1), (A,0), (B,34), (C,0), (D,0)]
        nextState resetState [(CpImm 34 B)] `shouldBe` expstate

      it "should execute instr and return new state for jnz branch taken" $ do
        let startstate = Map.fromList [(IP,0), (A,1), (B,0), (C,0), (D,0)]
            expstate   = Map.fromList [(IP,3), (A,1), (B,0), (C,0), (D,0)]
        nextState startstate [(JnzReg A 3)] `shouldBe` expstate

      it "should execute instr and return new state for jnz branch not taken" $ do
        let startstate = Map.fromList [(IP,0), (A,0), (B,0), (C,0), (D,0)]
            expstate   = Map.fromList [(IP,1), (A,0), (B,0), (C,0), (D,0)]
        nextState startstate [(JnzReg A 3)] `shouldBe` expstate

      it "should return correct final state for test input" $ do
        let instrs = List.map parseInstr testInput
        runUntil (\s -> (s ! IP) > 5) resetState instrs `shouldBe` Map.fromList [(IP,6),(A,42),(B,0),(C,0),(D,0)]

      it "should return correct final state for day 12 input" $ do
        let expstate = Map.fromList [(IP,23),(A,318083),(B,196418),(C,0),(D,0)]
            instrs = List.map parseInstr day12Input
        runUntil (\s -> (s ! IP) >= (length instrs)) resetState instrs `shouldBe` expstate

      it "should return correct final state for day 12 input part 2" $ do
        let expstate = Map.fromList [(IP,23),(A,9227737),(B,5702887),(C,0),(D,0)]
            instrs = List.map parseInstr day12Input
            startstate = Map.insert C 1 resetState
        runUntil (\s -> (s ! IP) >= (length instrs)) startstate instrs `shouldBe` expstate

main :: IO ()
main = hspec spec

testInput = [
  "cpy 41 a",
  "inc a",
  "inc a",
  "dec a",
  "jnz a 2",
  "dec a"]

day12Input = [
  "cpy 1 a",
  "cpy 1 b",
  "cpy 26 d",
  "jnz c 2",
  "jnz 1 5",
  "cpy 7 c",
  "inc d",
  "dec c",
  "jnz c -2",
  "cpy a c",
  "inc a",
  "dec b",
  "jnz b -2",
  "cpy c b",
  "dec d",
  "jnz d -6",
  "cpy 16 c",
  "cpy 17 d",
  "inc a",
  "dec d",
  "jnz d -2",
  "dec c",
  "jnz c -5"]

module Main where

import Test.Hspec
import Day5
import Data.List (isInfixOf)

spec :: Spec
spec = do
  describe "Day5" $ do
    context "isMatch" $ do
      it "should match valid hash" $ do
        isMatch "000008f82" `shouldBe` True

    context "getPasswdChar" $ do
      it "should return correct char" $ do
        getPasswdChar "000008f82" `shouldBe` '8'

    context "getPasswd" $ do
--      it "should return correct passwd for test input" $ do
--        getPasswd 3 3231929 "abc" `shouldBe` "18f"

      it "should return correct passwd for day 5 input" $ do
        getPasswd 8 0 day5Input `shouldBe` "f77a0e6e"

main :: IO ()
main = hspec spec

day5Input = "cxdnnyjw"

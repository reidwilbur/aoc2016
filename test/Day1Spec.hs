module Main where

import Test.Hspec
import Day1 as Day1

spec :: Spec
spec = do
  describe "Day1" $ do
    context "t1" $ do
      it "t1 should return 1" $ do
        Day1.t1 1 `shouldBe` 1

main :: IO ()
main = hspec spec


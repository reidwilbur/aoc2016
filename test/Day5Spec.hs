module Main where

import Test.Hspec
import Day5
import Data.ByteString

spec :: Spec
spec = do
  describe "Day5" $ do
    context "isMatch" $ do
      it "should match valid hash" $ do
        isMatch (pack [0x00,0x00,0x08,0xf8]) `shouldBe` True

    context "isMatch'" $ do
      it "should match valid hash" $ do
        isMatch' (pack [0x00,0x00,0x01,0x50]) `shouldBe` True

    context "getPasswdChar" $ do
      it "should return correct char" $ do
        getPasswdChar (pack [0x00,0x00,0x08,0xf8]) `shouldBe` '8'

    context "getPasswdChar'" $ do
      it "should return correct char tuple" $ do
        getPasswdChar' (pack [0x00,0x00,0x01,0x50]) `shouldBe` ('5',1)

    context "toChar" $ do
      it "should return correct char" $ do
        toChar (fromIntegral 15) `shouldBe` 'f'

    context "getPasswd" $ do
      it "should return correct passwd for test input" $ do
        getPasswd 3 3231929 "abc" `shouldBe` "18f"

      it "should return correct passwd for day 5 input" $ do
        getPasswd 8 0 day5Input `shouldBe` "f77a0e6e"

    context "getPasswd'" $ do
      it "should return correct passwd for test input" $ do
        getPasswd' 2 3231929 "abc" `shouldBe` "5e"

      it "should return correct passwd for test input" $ do
        getPasswd' 8 0 day5Input `shouldBe` "999828ec"

main :: IO ()
main = hspec spec

day5Input = "cxdnnyjw"

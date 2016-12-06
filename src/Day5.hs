module Day5
  ( getHash
  , isMatch
  , getPasswdChar
  , getPasswd
  ) where

import qualified Data.Hash.MD5 as MD5
import Data.List as List

getHash :: String -> Integer -> String
getHash s i = MD5.md5s $ MD5.Str $ s ++ show i

isMatch :: String -> Bool
isMatch ('0':'0':'0':'0':'0':_) = True
isMatch _ = False

getPasswdChar :: String -> Char
getPasswdChar = head . drop 5

getPasswd :: Integer -> Integer -> String -> String
getPasswd pwlen start base = List.take (fromIntegral pwlen) $ List.map getPasswdChar $ List.filter isMatch $ List.map (getHash base) [start..]


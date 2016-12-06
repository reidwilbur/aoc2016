module Day5
  ( getHash
  , isMatch
  , getPasswdChar
  , getPasswdChar'
  , getPasswd
  , getPasswd'
  , isMatch'
  , toChar
  ) where

import Crypto.Hash.MD5 as MD5
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Debug.Trace as Trace
import Data.Set as Set
import Data.Word
import Data.Bits
import Data.Maybe

getHash :: String -> Integer -> ByteString
getHash s i = MD5.hash $ BSC.pack $ s ++ show i

isMatch :: ByteString -> Bool
isMatch bs =
  let words = (BS.index bs 0, BS.index bs 1, ((BS.index bs 2) .&. 0xf0))
   in case (words) of
        (0,0,0) -> True
        _ -> False

toChar :: Word8 -> Char
toChar 0 = '0'
toChar 1 = '1'
toChar 2 = '2'
toChar 3 = '2'
toChar 4 = '4'
toChar 5 = '5'
toChar 6 = '6'
toChar 7 = '7'
toChar 8 = '8'
toChar 9 = '9'
toChar 10 = 'a'
toChar 11 = 'b'
toChar 12 = 'c'
toChar 13 = 'd'
toChar 14 = 'e'
toChar 15 = 'f'
toChar val = error ("couldn't convert word " ++ show val)


getPasswdChar :: ByteString -> Char
getPasswdChar bs = toChar $ (BS.index bs 2) .&. 0x0f

getPasswd :: Integer -> Integer -> String -> String
getPasswd pwlen start base = List.take (fromIntegral pwlen) $ List.map getPasswdChar $ List.filter isMatch $ List.map (getHash base) [start..]


isMatch' :: ByteString -> Bool
isMatch' bs =
  let words = (BS.index bs 0, BS.index bs 1, ((BS.index bs 2) .&. 0xf0))
      pos = ((BS.index bs 2) .&. 0x0f)
   in case (words, pos) of
        ((0,0,0), p) | p < 8 -> True
        _ -> False

getPasswdChar' :: ByteString -> (Char, Integer)
getPasswdChar' bs =
  let pos = (BS.index bs 2) .&. 0x0f
      charval = 0x0f .&. (rotateR (BS.index bs 3) 4)
   in (toChar charval, fromIntegral pos)

accumChars :: Int -> [(Char, Integer)] -> Set Integer -> [(Char, Integer)] -> [(Char, Integer)]
accumChars _ [] _ acc = acc
accumChars pwlen _ found acc | Set.size found == pwlen = acc
accumChars pwlen ((c@(_,ofs)):rest) found acc =
  if Set.member ofs found
     then accumChars pwlen rest found acc
     else accumChars pwlen rest (Set.insert ofs found) (c:acc)

getPasswd' :: Int -> Integer -> String -> String
getPasswd' pwlen start base =
  let validchars = List.map getPasswdChar' $ List.filter (isMatch') $ List.map (getHash base) [start..]
   in fmtPasswd $ accumChars pwlen validchars Set.empty []

fmtPasswd :: [(Char, Integer)] -> String
fmtPasswd = List.map (\(c, _) -> c) . List.sortBy (\(_, ofs1) (_, ofs2) -> compare ofs1 ofs2)


module Day4
  ( parseRoom
  , parseRooms
  , Room(..)
  , genCksum
  , getValidRooms
  , getSectorSum
  , decryptChar
  , decryptRoom
  ) where

import Text.Regex.Posix
import Data.Map as Map
import Data.List as List
import Data.Ord
import Data.Char as Char

data Room = Room { name :: String, sector :: Integer, cksum :: String } deriving (Eq, Show)

parseRoom :: String -> Room
parseRoom roomstr = let (_,_,_,(n:s:ck:_)) = (roomstr =~ "^([a-z-]+)-([0-9]+).([a-z]+).$" :: (String,String,String,[String]))
                     in Room n (read s) ck

parseRooms :: [String] -> [Room]
parseRooms = List.map parseRoom

sortTuples :: (Char,Integer) -> (Char,Integer) -> Ordering
sortTuples (c1,f1) (c2,f2) =
  case compare f1 f2 of
    EQ -> compare c1 c2
    LT -> GT
    GT -> LT

genCksum :: String -> String
genCksum s =
  let freqmap = List.foldr (\c fs -> Map.insertWith (+) c 1 fs) Map.empty s
      topfive = List.take 5 $ List.sortBy sortTuples $ Map.toList freqmap
   in List.map (\(char,_) -> char) topfive

getValidRooms :: [Room] -> [Room]
getValidRooms = List.filter (\r -> genCksum (List.filter (/='-') (name r)) == cksum r)

getSectorSum :: [Room] -> Integer
getSectorSum = List.sum . List.map (sector) . getValidRooms

decryptChar :: Integer -> Char -> Char
decryptChar i c =
  case c of
    '-' -> ' '
    _ -> let cidx = Char.ord c - Char.ord 'a'
             sftidx = (cidx + fromIntegral i) `mod` 26
             sftalign = sftidx + Char.ord 'a'
          in Char.chr sftalign

decryptRoom :: Room -> Room
decryptRoom r = let decname = List.map (decryptChar (sector r)) $ name r
                 in Room decname (sector r) (cksum r)


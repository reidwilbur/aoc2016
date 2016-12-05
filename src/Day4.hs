module Day4
  ( parseRoom
  , parseRooms
  , Room(..)
  , genCksum
  , getValidRooms
  , getSectorSum
  ) where

import Text.Regex.Posix
import Data.Map as Map
import Data.List as List
import Data.Ord

data Room = Room { name :: String, sector :: Integer, cksum :: String } deriving (Eq, Show)

parseRoom :: String -> Room
parseRoom roomstr = let (_,_,_,(n:s:ck:_)) = (roomstr =~ "^([a-z-]+)([0-9]+).([a-z]+).$" :: (String,String,String,[String]))
                     in Room (List.filter (/='-') n) (read s) ck

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
getValidRooms = List.filter (\r -> genCksum (name r) == cksum r)

getSectorSum :: [Room] -> Integer
getSectorSum = List.sum . List.map (sector) . getValidRooms


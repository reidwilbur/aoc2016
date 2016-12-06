module Day6
  ( decode
  , mostCommon
  , leastCommon
  ) where

import Data.Map as Map
import Data.List as List

type ColCharFreq = ((Integer, Char), Integer)

mostCommon :: ColCharFreq -> ColCharFreq -> Ordering
mostCommon (_, f1) (_, f2) = compare f2 f1

leastCommon :: ColCharFreq -> ColCharFreq -> Ordering
leastCommon (_, f1) (_, f2) = compare f1 f2

decode :: (ColCharFreq -> ColCharFreq -> Ordering) -> [String] -> String
decode sorter ss =
      -- [(col, char)]
  let colchars = List.concat $ List.map (List.zip [0..]) ss
      -- [((col, char), freq)] unique (col, char)
      colcharfreq = Map.toList $ List.foldr (\colchar m -> Map.insertWith (+) colchar 1 m) Map.empty colchars
      -- [[((1, c), 1)..],[((2, d), 3)..]]  grouped by column num
      colchargroup = List.groupBy (\((col1, _), _) ((col2, _), _) -> col1 == col2) colcharfreq
      -- same as above just sorted by frequency in column groups
      colcharsort = List.map (List.sortBy sorter) colchargroup
   in List.map (\((_, c), _) -> c) $ List.sort $ List.map (List.head) colcharsort

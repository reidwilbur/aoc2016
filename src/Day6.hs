module Day6
  ( decode
  ) where

import Data.Map as Map
import Data.List as List

decode :: [String] -> String
decode ss =
      -- [(col, char)]
  let colchars = List.concat $ List.map (List.zip [0..]) ss
      -- [((col, char), freq)] unique (col, char)
      colcharfreq = Map.toList $ List.foldr (\colchar m -> Map.insertWith (+) colchar 1 m) Map.empty colchars
      -- [[((1, c), 1)..],[((2, d), 3)..]]  grouped by column num
      colchargroup = List.groupBy (\((col1, _), _) ((col2, _), _) -> col1 == col2) colcharfreq
      -- same as above just sorted by frequency
      colcharsort = List.map (List.sortBy (\(_, f1) (_, f2) -> compare f2 f1)) colchargroup
   in List.map (\((_, c), _) -> c) $ List.sort $ List.map (List.head) colcharsort

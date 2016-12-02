module Day1 (getHqDist, parseInput, parseStep, PathStep(R,L)) where

import Data.List.Split as Split
import Data.List as List

data PathStep = R Int
              | L Int
              deriving (Eq, Ord, Show)

data Direction = N | S | E | W deriving (Eq, Ord, Show)

type Position = (Direction, (Int, Int))

parseStep :: String -> PathStep
parseStep s = case s of
                'L':d -> L (read d :: Int)
                'R':d -> R (read d :: Int)
                _ -> error ("could not parse step " ++ s)

parseInput :: String -> [PathStep]
parseInput "" = []
parseInput ss = List.map parseStep $ Split.splitOn ", " ss

getNextPos :: Position -> PathStep -> Position
getNextPos (N, (x, y)) (R i) = (E, (x + i, y))
getNextPos (N, (x, y)) (L i) = (W, (x - i, y))
getNextPos (E, (x, y)) (R i) = (S, (x, y - i))
getNextPos (E, (x, y)) (L i) = (N, (x, y + i))
getNextPos (S, (x, y)) (R i) = (W, (x - i, y))
getNextPos (S, (x, y)) (L i) = (E, (x + i, y))
getNextPos (W, (x, y)) (R i) = (N, (x, y + i))
getNextPos (W, (x, y)) (L i) = (S, (x, y - i))

getHqDist :: String -> Int
getHqDist [] = 0
getHqDist ss = let (d, (fx, fy)) = List.foldl getNextPos (N, (0,0)) $ parseInput ss
                in (abs fx) + (abs fy)


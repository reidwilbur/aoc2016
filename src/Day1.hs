module Day1 (
  getFirstCoordVisitedTwice,
  getCoordDist,
  getHqDist,
  parseInput,
  parseStep,
  getPath,
  PathStep(R,L),
  Direction(N,E,S,W)) where

import Data.List.Split as Split
import Data.List as List
import Data.Set as Set
import Data.Maybe

data PathStep = R Int
              | L Int
              deriving (Eq, Show)

data Direction = N | E | S | W deriving (Eq, Enum, Show)

type Coord = (Int, Int)

type Position = (Direction, Coord)

parseStep :: String -> PathStep
parseStep s = case s of
                'L':d -> L (read d :: Int)
                'R':d -> R (read d :: Int)
                _ -> error ("could not parse step " ++ s)

parseInput :: String -> [PathStep]
parseInput "" = []
parseInput ss = List.map parseStep $ Split.splitOn ", " ss

getDist :: PathStep -> Int
getDist (R i) = i
getDist (L i) = i

turn :: Direction -> PathStep -> Direction
turn W (R _) = N
turn N (L _) = W
turn d (R _) = succ d
turn d (L _) = pred d

advance :: Coord -> Direction -> Int -> Coord
advance (x, y) N i = (x,     y + i)
advance (x, y) S i = (x,     y - i)
advance (x, y) E i = (x + i, y    )
advance (x, y) W i = (x - i, y    )

walkPath :: Position -> PathStep -> [Position]
walkPath (dir, coord) step =
  let newdir = turn dir step
      dist = getDist step
   in reverse $ List.map (\crd -> (newdir, crd)) $ List.map (advance coord newdir) $ [1..dist]

getHqDist :: [PathStep] -> Int
getHqDist [] = 0
getHqDist pss = let (_, coord) = last $ getPath [(N, (0,0))] pss
                 in getCoordDist coord

getPath :: [Position] -> [PathStep] -> [Position]
getPath [] _ = error "Must have initial position in position list"
getPath pstns [] = reverse pstns
getPath pstns (ps:pss) = let newpstns = walkPath (head pstns) ps
                          in getPath (newpstns ++ pstns) pss

getFirstCoordVisitedTwice :: [Position] -> Set Coord -> Maybe Coord
getFirstCoordVisitedTwice [] _ = Nothing
getFirstCoordVisitedTwice ((_, coord):pstns) visited =
  if Set.member coord visited
     then Just coord
     else getFirstCoordVisitedTwice pstns (Set.insert coord visited)

getCoordDist :: Coord -> Int
getCoordDist (x, y) = (abs x) + (abs y)

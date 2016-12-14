module Day13
  ( getNeighbors
  , bfs
  , bfs50
  , isOpen
  ) where

import Data.Bits as Bits
import qualified Data.Set as Set

type Position = (Int, Int)

isOpen :: Int -> Position -> Bool
isOpen favnum (x, y) = even $ Bits.popCount ((x*x) + (3*x) + (2*x*y) + y + (y*y) + favnum)

getNeighbors :: Int -> Int -> Position -> Set.Set Position -> [(Int, Position)]
getNeighbors favnum step (x,y) seen =
  let pnbors = (x+1,y):(x-1,y):(x,y+1):(x,y-1):[]
      vnbors = filter (\p -> Set.notMember p seen) $ filter (isOpen favnum) $ filter (\(x,y) -> (x>=0) && (y>=0)) pnbors
   in map (\p -> (step, p)) vnbors

bfs :: Int -> Position -> Set.Set Position -> [(Int, Position)] -> Int
bfs _ _ _ [] = error "no path to pos"
bfs _ pos _ ((steps, nbor):_) | nbor == pos = steps
bfs favnum pos seen ((steps, nbor):rest) =
  let nseen = Set.insert nbor seen
      nbors = rest ++ (getNeighbors favnum (steps+1) nbor nseen)
   in bfs favnum pos nseen nbors

bfs50 :: Int -> Set.Set Position -> [(Int, Position)] -> Set.Set Position
bfs50 _ seen [] = seen
bfs50 _ seen ((steps, nbor):_) | steps > 50 = seen
bfs50 favnum seen ((steps, nbor):rest) =
  let nseen = Set.insert nbor seen
      nbors = rest ++ (getNeighbors favnum (steps+1) nbor nseen)
   in bfs50 favnum nseen nbors


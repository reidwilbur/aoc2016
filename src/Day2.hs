module Day2 (
  parseInstrs,
  getCode,
  getCodeRot,
  getFinalPosition,
  nextPos9sq,
  nextPos25sq,
  Instr(U,L,D,R)
  ) where

import Data.Set as Set
import Data.List as List
import Numeric (showHex)

data Instr = U | L | D | R deriving(Eq, Show)

type Position = (Integer, Integer)

type NextPosFn = (Instr -> Position -> Position)

validRotPos = Set.fromList [(0,2), (1,1), (1,2), (1,3), (2,0), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (4,2)]

validPos = Set.fromList [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2)]

nextPos :: Set Position -> Instr -> Position -> Position
nextPos validpos instr pos@(r, c) =
  let np = case (instr) of
             U -> (r - 1, c)
             D -> (r + 1, c)
             L -> (r,     c - 1)
             R -> (r,     c + 1)
  in if Set.member np validpos then np else pos

parseChar :: Char -> Instr
parseChar c =
  case (c) of
    'U' -> U
    'L' -> L
    'D' -> D
    'R' -> R
    _ -> error ("Can't parse char " ++ show c)

parseLine :: String -> [Instr]
parseLine = List.foldr (\c il -> (parseChar c):il) []

parseInstrs :: [String] -> [[Instr]]
parseInstrs = List.foldr (\s ils -> (parseLine s):ils) []

getFinalPosition :: NextPosFn -> Position -> [Instr] -> Position
getFinalPosition nextposfn p = head . List.foldl (\poss instr -> (nextposfn instr (head poss)):poss) [p]

getPositions :: Position -> NextPosFn -> [[Instr]] -> [Position]
getPositions start nextposfn ils = reverse $ init $ List.foldl (\poss instrs -> (getFinalPosition nextposfn (head poss) instrs):poss) [start] ils

nextPos9sq = nextPos validPos

nextPos25sq = nextPos validRotPos

getCode :: [[Instr]] -> [Integer]
getCode ils =
  let pstns = getPositions (1,1) nextPos9sq ils
  in List.map (\(r, c) -> (r * 3) + c + 1) pstns

getCodeRot :: [[Instr]] -> [String]
getCodeRot ils =
  let pstns = getPositions (2,0) nextPos25sq ils
  in List.map (\p -> showHex ((Set.findIndex p validRotPos) + 1) "") pstns


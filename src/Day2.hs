module Day2 (
  parseInstrs,
  getCode,
  getFinalPosition,
  Instr(U,L,D,R)
  ) where

data Instr = U | L | D | R deriving(Eq, Show)

type Position = (Int, Int)

nextPos :: Instr -> Position -> Position
nextPos instr pos@(r, c) =
  case (instr) of
    U -> if (r > 0) then (r - 1, c) else pos
    D -> if (r < 2) then (r + 1, c) else pos
    L -> if (c > 0) then (r,     c - 1) else pos
    R -> if (c < 2) then (r,     c + 1) else pos

parseChar :: Char -> Instr
parseChar c =
  case (c) of
    'U' -> U
    'L' -> L
    'D' -> D
    'R' -> R
    _ -> error ("Can't parse char " ++ show c)

parseLine :: String -> [Instr]
parseLine = foldr (\c il -> (parseChar c):il) []

parseInstrs :: [String] -> [[Instr]]
parseInstrs = foldr (\s ils -> (parseLine s):ils) []

getFinalPosition :: Position -> [Instr] -> Position
getFinalPosition p = head . foldl (\poss instr -> (nextPos instr (head poss)):poss) [p]

getPositions :: [[Instr]] -> [Position]
getPositions = reverse . init . foldl (\poss instrs -> (getFinalPosition (head poss) instrs):poss) [(1,1)]

getCode :: [[Instr]] -> [Int]
getCode = map (\(r, c) -> (r * 3) + c + 1) . getPositions

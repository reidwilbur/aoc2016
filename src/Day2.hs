module Day2 (
  parseInstrs,
  Instr(U,L,D,R)
  ) where

data Instr = U | L | D | R deriving(Eq, Show)

parseChar :: Char -> Instr
parseChar c =
  case (c) of
    'U' -> U
    'L' -> L
    'D' -> D
    'R' -> R
    _ -> error ("Can't parse char " ++ show c)

parseLine :: String -> [Instr]
parseLine s = foldr (\c il -> (parseChar c):il) [] s

parseInstrs :: [String] -> [[Instr]]
parseInstrs ss = foldr (\s ils -> (parseLine s):ils) [] ss


module Day9
  ( decompress
  , parseInstr
  , decompressLen
  ) where

import qualified Data.ByteString.Char8 as BS

decompress :: String -> String
decompress [] = []
decompress ('(':rest) =
  let (instr, rest') = span (/=')') rest
      (len, rep) = parseInstr instr
      (word, rest'') = splitAt len $ dropWhile (==')') rest'
   in (concat (replicate rep word)) ++ decompress rest''
decompress (c:rest) = c:(decompress rest)

decompressLen :: String -> Int
decompressLen "" = 0
decompressLen ('(':rest) =
  let (instr, rest') = span (/=')') rest
      (len, rep) = parseInstr instr
      (word, rest'') = splitAt len $ dropWhile (==')') rest'
      dcmpwordlen = decompressLen word
   in (rep * dcmpwordlen) + decompressLen rest''
decompressLen (c:rest) = 1 + (decompressLen rest)

parseInstr :: String -> (Int, Int)
parseInstr s =
  let len = read $ takeWhile (/='x') s
      rep = read $ dropWhile (=='x') $ dropWhile (/='x') s
   in (len, rep)


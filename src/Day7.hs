module Day7
  ( parseSeqs
  , parseAddress
  , supportsTLS
  , supportsSSL
  , Address(..)
  ) where

import Data.List as List

data Address = Address { seqs :: [String], hnets :: [String] } deriving (Eq, Show)

type ABA = (Char,Char)

parseSeqs :: String -> [String] -> [String] -> ([String], [String])
parseSeqs "" seqs hnets = (reverse $ filter (/="") seqs, reverse $ filter (/="") hnets)
parseSeqs s seqs hnets =
  let (seq, next) = span (/='[') s
      (hnet, rest) = span (/=']') $ dropWhile (=='[') next
   in parseSeqs (dropWhile (==']') rest) (seq:seqs) (hnet:hnets)

parseAddress :: String -> Address
parseAddress s = let (sqs, hnts) = parseSeqs s [] []
                  in Address { seqs=sqs, hnets=hnts }

seqHasABBA :: String -> Bool
seqHasABBA (a:b:c:d:rest) = if ((a /= b) && (a == d) && (b == c)) then True else seqHasABBA (b:c:d:rest)
seqHasABBA _ = False

getABAs :: String -> [ABA]
getABAs (a:b:c:rest) =
  if (a == c) then (a,b):getABAs (b:c:rest) else getABAs (b:c:rest)
getABAs _ = []

hasBAB :: ABA -> String -> Bool
hasBAB (aa,bb) s = List.isInfixOf (bb:aa:bb:[]) s

supportsSSL :: Address -> Bool
supportsSSL addr =
  let abas = concat $ map (getABAs) $ seqs addr
      hnts = hnets addr
   in [] /= filter (\hnet -> [] /= (filter (==True) $ map (\aba -> hasBAB aba hnet) abas)) hnts

supportsTLS :: Address -> Bool
supportsTLS addr =
  let hnetabbas = filter seqHasABBA $ hnets addr
      seqabbas = filter seqHasABBA $ seqs addr
   in (hnetabbas == []) && (seqabbas /= [])


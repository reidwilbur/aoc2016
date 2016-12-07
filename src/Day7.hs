module Day7
  ( parseSeqs
  , parseAddress
  , supportsTLS
  , Address(..)
  ) where

data Address = Address { seqs :: [String], hnets :: [String] } deriving (Eq, Show)

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

supportsTLS :: Address -> Bool
supportsTLS addr =
  let hnetabbas = filter seqHasABBA $ hnets addr
      seqabbas = filter seqHasABBA $ seqs addr
   in (hnetabbas == []) && (seqabbas /= [])


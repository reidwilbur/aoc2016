module Day3 (
  getValidTris,
  isValidTri,
  toTris,
  txposeTriList,
  ) where

type Tri = (Integer, Integer, Integer)

isValidTri :: Tri -> Bool
isValidTri (a, b, c) =
  a + b > c
  && a + c > b
  && b + c > a

getValidTris :: [Tri] -> [Tri]
getValidTris = filter isValidTri

toTris :: [Integer] -> [Tri]
toTris (a:b:c:rest) = (a,b,c) : (toTris rest)
toTris [] = []
toTris _ = error "List length is not multiple of 3"

txposeTriList :: [Tri] -> [Tri]
txposeTriList tris = let (la, lb, lc) = foldl (\(l1,l2,l3) (a,b,c) -> (a:l1, b:l2, c:l3)) ([], [], []) tris
                         lls = concat [reverse la, reverse lb, reverse lc]
                      in toTris lls

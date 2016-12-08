module Day8
  ( Instr(..)
  , parseInstr
  , Executable(..)
  , Screen
  , toString
  ) where

import qualified Data.Vector as Vect

type Screen = Vect.Vector (Vect.Vector Bool)

data Instr = Rect { cols :: Int, rows :: Int }
           | RotR { row :: Int, cols :: Int }
           | RotC { col :: Int, rows :: Int }
           deriving(Eq, Show)

class Executable a where
  exec :: a -> Screen -> Screen

instance Executable Instr where
  exec instr@(Rect _ _) scrn =
    let rowvals = map (\ci -> (ci, True)) $ [0..(cols instr) - 1]
     in Vect.imap (\idx vect -> if idx < (rows instr) then (vect Vect.// rowvals) else vect) scrn

  exec instr@(RotR _ _) scrn =
    let vect = scrn Vect.! (row instr)
        rotated = rotateR vect (cols instr)
     in scrn Vect.// [(row instr, rotated)]

  exec instr@(RotC _ _) scrn =
    let cl = (col instr)
        colvect = Vect.foldr (\rowvect cvct -> (rowvect Vect.! cl) `Vect.cons` cvct) (Vect.empty) scrn
        rotated = rotateR colvect (rows instr)
     in Vect.zipWith (\v rowv -> rowv Vect.// [(cl,v)]) rotated scrn

rotateR :: Vect.Vector a -> Int -> Vect.Vector a
rotateR vect cols =
  let cs = cols `mod` (Vect.length vect)
      tl = Vect.drop ((Vect.length vect) - cs) vect
      ft = Vect.take ((Vect.length vect) - cs) vect
   in tl Vect.++ ft

toString :: Screen -> String
toString = Vect.foldr (\rowvect tstr -> ((Vect.foldr (\v rstr -> if v == True then '@':rstr else ' ':rstr)) "" rowvect) ++ "\n" ++ tstr) ""

parseInstr :: String -> Instr
parseInstr s =
  case (words s) of
    ("rect":rest:[]) -> let (cols, rows) = break (=='x') rest
                         in Rect (read cols) (read (dropWhile (=='x') rows))
    ("rotate":"row":('y':'=':r):"by":cols:[]) -> RotR (read r) (read cols)
    ("rotate":"column":('x':'=':c):"by":rows:[]) -> RotC (read c) (read rows)



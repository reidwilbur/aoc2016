module Day12
  ( Reg(..)
  , Instr(..)
  , RegState
  , resetState
  , nextState
  , parseInstr
  , runUntil
  ) where

import Data.Map.Strict as Map

data Reg  = IP | A | B | C | D deriving(Eq, Ord, Show)

data Instr = CpImm { srcI :: Int, dst :: Reg }
           | CpReg { srcR :: Reg, dst :: Reg }
           | Inc { reg :: Reg }
           | Dec { reg :: Reg }
           | JnzReg { reg :: Reg, ofs :: Int }
           | JnzImm { imm :: Int, ofs :: Int }
           deriving(Eq, Show)

type RegState = Map.Map Reg Int

class Executable a where
  exec :: RegState -> a -> RegState

updateIP :: RegState -> RegState
updateIP = Map.insertWith (+) IP 1

instance Executable Instr where
  exec state (CpImm v r)     = updateIP $ Map.insert r v state
  exec state (CpReg s d) = updateIP $ Map.insert d (state ! s) state
  exec state (Inc r)         = updateIP $ Map.insertWith (+) r 1 state
  exec state (Dec r)         = updateIP $ Map.insertWith (\n o -> o - n) r 1 state
  exec state (JnzReg r o)  = if (state ! r) /= 0 then Map.insertWith (+) IP o state else updateIP state
  exec state (JnzImm v o)  = if (v /= 0)        then Map.insertWith (+) IP o state else updateIP state

resetState = Map.fromList [(IP,0), (A,0), (B,0), (C,0), (D,0)] :: RegState

nextState :: RegState -> [Instr] -> RegState
nextState state instrs = exec state (instrs !! (state ! IP))

parseReg :: String -> Reg
parseReg "a" = A
parseReg "b" = B
parseReg "c" = C
parseReg "d" = D
parseReg r = error ("couldn't parse reg " ++ r)

parseInstr :: String -> Instr
parseInstr s =
  case (words s) of
    ("cpy":"a":dst:[]) -> CpReg A $ parseReg dst
    ("cpy":"b":dst:[]) -> CpReg B $ parseReg dst
    ("cpy":"c":dst:[]) -> CpReg C $ parseReg dst
    ("cpy":"d":dst:[]) -> CpReg D $ parseReg dst
    ("cpy":v:dst:[]) -> CpImm (read v) (parseReg dst)
    ("inc":r:[]) -> Inc $ parseReg r
    ("dec":r:[]) -> Dec $ parseReg r
    ("jnz":"a":o:[]) -> JnzReg A (read o)
    ("jnz":"b":o:[]) -> JnzReg B (read o)
    ("jnz":"c":o:[]) -> JnzReg C (read o)
    ("jnz":"d":o:[]) -> JnzReg D (read o)
    ("jnz":i:o:[]) -> JnzImm (read i) (read o)
    _ -> error ("could not parse instr " ++ s)

runUntil :: (RegState -> Bool) -> RegState -> [Instr] -> RegState
runUntil fn state instrs =
  if (fn state)
     then state
     else runUntil fn (nextState state instrs) instrs


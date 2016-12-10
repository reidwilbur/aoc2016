module Day10
  ( parseInstr
  , nextState
  , initState
  , runUntil
  , Instr(..)
  , Dest(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

data Dest = Bot Int | Output Int deriving(Eq, Show)

data Instr = Value { bot :: Int, val :: Int }
           | Cmd { bot :: Int
                 , lo :: Dest
                 , hi :: Dest }
           deriving(Eq, Show)

type BotState = Map.Map Int [Int]
type OutState = Map.Map Int [Int]

type InstrMap = Map.Map Int Instr

parseInstr :: String -> Instr
parseInstr s =
  let wrds = words s
   in case wrds of
        ("value":v:_:_:_:b:[]) -> Value { bot = read b, val = read v }
        ("bot":b:_:_:_:ldest:ld:_:_:_:hdest:hd:[]) ->
          Cmd { bot = read b, lo = (parseDest ldest ld), hi = (parseDest hdest hd) }
            where parseDest name dest = if name == "bot" then Bot (read dest) else Output (read dest)
        _ -> error ("coudn't parse string: " ++ s)

isValue :: Instr -> Bool
isValue (Value _ _) = True
isValue _ = False

initState :: [Instr] -> (BotState, OutState, InstrMap)
initState instrs =
  let (vals, cmds) = List.partition isValue instrs
      botstate = foldr (\v bs -> Map.insertWith (\nv ov -> nv ++ ov) (bot v) [val v] bs) Map.empty vals
      outstate = Map.empty
      instrmap = foldr (\cmd im -> Map.insert (bot cmd) cmd im) Map.empty cmds
   in (botstate, outstate, instrmap)

execCmd :: Instr -> (BotState, OutState) -> (BotState, OutState)
execCmd cmd (bs, os) =
  let botvals = bs Map.! (bot cmd)
      lval = List.minimum botvals
      hval = List.maximum botvals
      (bs', os') =
        case (lo cmd) of
          Bot b -> (Map.insertWith (\nv ov -> nv ++ ov) b [lval] bs, os)
          Output o -> (bs, Map.insertWith(\nv ov -> nv ++ ov) o [lval] os)
   in case (hi cmd) of
        Bot b -> (Map.insertWith (\nv ov -> nv ++ ov) b [hval] bs', os')
        Output o -> (bs', Map.insertWith(\nv ov -> nv ++ ov) o [hval] os')

nextState :: (BotState, OutState, InstrMap) -> (BotState, OutState, InstrMap)
nextState (bs, os, imap) =
  let cmds = Map.elems $ Map.filterWithKey (\k _ -> (Map.member k bs) && (2 == (length $ bs Map.! k))) imap
      (nbs, nos) = List.foldr execCmd (bs, os) cmds
      resolvedbots = List.map (bot) cmds
      nimap = Map.filterWithKey (\k _ -> k `List.notElem` resolvedbots) imap
   in (nbs, nos, nimap)

runUntil :: ((BotState, OutState, InstrMap) -> Bool) -> (BotState, OutState, InstrMap) -> (BotState, OutState, InstrMap)
runUntil fin state =
  if fin state
     then state
     else runUntil fin (nextState state)


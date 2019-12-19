import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.List (sortBy)

import IntCode
import Debug.Trace


main :: IO ()
main = do
  program <- memoryFromInputFile "15"
  let finalLayout = layout $ execState (runActions getNextDirection getStatus program) startState
  print finalLayout

type Layout = Map Pos (Tile, Int)
type Pos = (Int, Int)
data Tile = Empty | Wall | Oxygen
  deriving (Show, Eq)
data Direction = N | W | S | E
  deriving (Show, Eq)

type Runner = State RunnerState
data RunnerState = RunnerState
  { curPos :: Pos
  , lastMove :: Direction
  , layout :: Layout
  } deriving Show

startState :: RunnerState
startState = RunnerState
  { curPos = (0,0)
  , lastMove = N -- whatever
  , layout = Map.empty
  }

getNextDirection :: Runner Integer
getNextDirection = do
  d <- nextDirection
  modify (\st -> st { lastMove = d }) 
  return (directionToInt d)

directionToInt :: Direction -> Integer
directionToInt d = case d of
  N -> 1
  S -> 2
  W -> 3
  E -> 4

nextDirection :: Runner Direction
nextDirection = do
  st <- get
  traceShowM (Map.size . layout $ st, curPos st, lastMove st)
  let p = curPos st
      l = layout st
      moves = map (id &&& (\d -> maybe 0 snd . Map.lookup (move d p) $ l)) [N,S,W,E]
      leastSeen = head . sortBy (comparing snd) $ moves
  return (fst leastSeen)

getStatus :: Integer -> Runner ()
getStatus i = case i of
  0 -> do
    st <- get
    let triedPos = move (lastMove st) (curPos st)
        newLayout = addAt triedPos Wall (layout st)
    put st { layout = newLayout }
  1 -> do
    st <- get
    let newPos = move (lastMove st) (curPos st)
        newLayout = addAt newPos Empty (layout st)
    put st { curPos = newPos, layout = newLayout }
  2 -> do
    st <- get
    let newPos = move (lastMove st) (curPos st)
        newLayout = addAt newPos Oxygen (layout st)
    put st { curPos = newPos, layout = newLayout }
  n -> error $ "Unknown drone status " ++ show n

addAt :: Pos -> Tile -> Layout -> Layout
addAt p t l = Map.alter (Just . maybe (t, 0) (\(t', n) -> if t == t' then (t, n+1) else error ("Tiles don't match at position " ++ show p))) p l

move :: Direction -> Pos -> Pos
move d (x, y) = case d of
  N -> (x, y-1)
  S -> (x, y+1)
  W -> (x-1, y)
  E -> (x+1, y)

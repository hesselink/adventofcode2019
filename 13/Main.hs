module Main (main, printScreen, getManualInput {- useful but not used right now -}) where

import IntCode
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.IORef

main :: IO ()
main = do
  program <- memoryFromInputFile "13"
  let os = exec [] program
      blockCount = length . filter (== 2) . map last . chunksOf 3 $ os
  print blockCount
  ts <- newIORef Map.empty
  buf <- newIORef []
  _ <- runActions (getAutoInput ts) (getOutput buf ts) (setAt (Address 0) 2 program)
  printScore ts

getManualInput :: IORef Tiles -> IO Integer
getManualInput tsRef = do
  printScreen tsRef
  c <- getChar
  if c == 'h'
  then return (-1)
  else if c == 'l'
  then return 1
  else if c == ' '
  then return 0
  else getManualInput tsRef

getAutoInput :: IORef Tiles -> IO Integer
getAutoInput tsRef = do
  ts <- Map.toList <$> readIORef tsRef
  let ballPos = fst (findTile Ball ts)
      paddlePos = fst (findTile Paddle ts)
      move = if fst ballPos > fst paddlePos
             then 1 else
             if fst ballPos < fst paddlePos
             then -1
             else 0
  return move

findTile :: TileId -> [Tile] -> Tile
findTile tid ts = head $ dropWhile ((/= tid) . snd) ts

printScore :: IORef Tiles -> IO ()
printScore tsRef = do
  ts <- readIORef tsRef
  let Just (Score s) = Map.lookup (0, -1) ts
  print s

printScreen :: IORef Tiles -> IO ()
printScreen tsRef = do
  ts <- readIORef tsRef
  putStrLn (showScreen (Map.toList ts))

getOutput :: IORef [Integer] -> IORef Tiles -> Integer -> IO ()
getOutput buf tsRef i = do
  ts <- readIORef tsRef
  prev <- readIORef buf
  if length prev == 2
  then do
    writeIORef buf []
    writeIORef tsRef (uncurry Map.insert (parseTile (reverse (i:prev))) ts)
  else writeIORef buf (i:prev)

type Tile = (Pos, TileId)
type Tiles = Map Pos TileId
type Pos = (Integer, Integer)
data TileId = Empty | Wall | Block | Paddle | Ball | Score Integer
  deriving (Show, Eq)

showScreen :: [Tile] -> String
showScreen ts =
  let (xs, ys) = unzip (map fst ts)
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in showScreen' minX maxX minY maxY (Map.fromList ts)

showScreen' :: Integer -> Integer -> Integer -> Integer -> Tiles -> String
showScreen' minX maxX minY maxY ts
  = intercalate "\n"
  $ [ showLine minX maxX y ts | y <- [minY..maxY] ]

showLine :: Integer -> Integer -> Integer -> Tiles -> String
showLine minX maxX y ts = concat [ maybe " " showTile $ Map.lookup (x,y) ts | x <- [minX..maxX] ]

parseTile :: [Integer] -> Tile
parseTile [-1,0,s] = ((0,-1), Score s)
parseTile [x,y,t] = ((x,y), parseTileId t)
parseTile _ = error "parseTile needs three input items"

parseTileId :: Integer -> TileId
parseTileId 0 = Empty
parseTileId 1 = Wall
parseTileId 2 = Block
parseTileId 3 = Paddle
parseTileId 4 = Ball
parseTileId x = error $ "unknown tile id: " ++ show x

showTile :: TileId -> String
showTile Empty = " "
showTile Wall = "#"
showTile Block = "x"
showTile Paddle = "_"
showTile Ball = "*"
showTile (Score s) = show s

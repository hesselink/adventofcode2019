{-# LANGUAGE TupleSections #-}
import Data.List.Split
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Control.Arrow (first, second)
import Data.Map (Map)
import Data.Set (Set)
import Data.List
import Data.Ord
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  f <- readFile "input/3"
  let stepsLines = map parseSteps . lines $ f
      positions = map allPositions stepsLines
      wires = Map.unionsWith Set.union $ map (uncurry wireFromIndexAndPositions) (zip [0..] positions)
      closest = minimumBy (comparing manhattanDistance)
              . map fst
              . filter ((> 1) . Set.size . snd)
              . Map.toList
              $ wires
  print (manhattanDistance closest)
  let wires2 = Map.unionsWith merge $ map (uncurry wireFromPositionWithDistance) (zip [0..] $ map (flip zip [1..]) positions)
      merge (s1, d1) (s2, d2) = (Set.union s1 s2, d1 + d2)
      closest2 = minimumBy (comparing (snd . snd))
               . filter ((> 1) . Set.size . fst . snd)
               . Map.toList
               $ wires2
  print (snd $ snd closest2)
  return ()

data Step = Step Direction Int
  deriving Show

data Direction = R | U | L | D
  deriving Show

type Wires = Map Pos
type WiresWithIndex = Wires (Set WireId)
type WiresWithDistance = Wires (Set WireId, Int)

type Pos = (Int, Int)
type WireId = Int

manhattanDistance :: Pos -> Int
manhattanDistance (x, y) = abs x + abs y

wireFromIndexAndPositions :: Int -> [Pos] -> WiresWithIndex
wireFromIndexAndPositions ix = Map.fromList . map (, Set.singleton ix)

wireFromPositionWithDistance :: Int -> [(Pos, Int)] -> WiresWithDistance
wireFromPositionWithDistance ix = Map.fromList . map (second (Set.singleton ix, ))

-- Positions

allPositions :: [Step] -> [Pos]
allPositions = go (0,0) . concatMap stepFunctions
  where
    go _ [] = []
    go pos (f:fs) = let newPos = f pos in newPos : go newPos fs

stepFunctions :: Step -> [Pos -> Pos]
stepFunctions (Step dir num) = replicate num (stepFunction dir)

stepFunction :: Direction -> Pos -> Pos
stepFunction R = first (+1)
stepFunction U = second (+1)
stepFunction L = first (subtract 1)
stepFunction D = second (subtract 1)

-- Parsing

parseSteps :: String -> [Step]
parseSteps = map (fromJust . parseStep) . splitOn ","

parseStep :: String -> Maybe Step
parseStep (c:ns) = Step <$> parseDirection c <*> parseNumber ns
parseStep _ = Nothing

parseDirection :: Char -> Maybe Direction
parseDirection c = case c of
  'R' -> Just R
  'U' -> Just U
  'L' -> Just L
  'D' -> Just D
  _   -> Nothing

parseNumber :: String -> Maybe Int
parseNumber = readMaybe

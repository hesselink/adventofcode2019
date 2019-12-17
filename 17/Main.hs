import IntCode
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  program <- memoryFromInputFile "17"
  let out = exec [] program
      ts = map (map parseTile) . splitOn [10] $ out
      withCoord = concatMap (\(y, l) -> zipWith (\x l' -> ((x, y), l')) [0..] l) (zip [0..] ts)
      tiles = Map.fromList withCoord
      intersections = filter (isIntersection tiles) withCoord
  print (sum $ map (alignmentParam . fst) intersections)
  return ()

type Pos = (Int, Int)
data Tile = Empty | Scaffold | Robot
  deriving (Eq)

instance Show Tile where
  show Scaffold = "#"
  show Empty = "."
  show Robot = "^"

parseTile :: Integer -> Tile
parseTile 35 = Scaffold
parseTile 46 = Empty
parseTile 94 = Robot
parseTile n = error $ "unknown tile: " ++ show n

isIntersection :: Map Pos Tile -> (Pos, Tile) -> Bool
isIntersection ts (p, t) = t /= Empty && all (`elem` [Just Scaffold, Just Robot]) (map (flip Map.lookup ts) (neighbors p))

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

alignmentParam :: Pos -> Int
alignmentParam (x, y) = x * y

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import IntCode (memoryFromInputFile, exec, Memory)
import Data.List (intercalate)

main :: IO ()
main = do
  program <- memoryFromInputFile "11"
  print $ Set.size . Set.unions . runRobot Set.empty $ program
  putStrLn $ printPanels . last . runRobot (Set.singleton (0,0)) $ program

type Pos = (Int, Int)
type Panels = Set Pos -- not present => black
data Direction = U | R | D | L
  deriving (Show, Eq)
data Robot = Robot
  { position :: Pos
  , direction :: Direction
  } deriving Show

printPanels :: Panels -> String
printPanels panels =
  let ps = Set.toList panels
      (xs, ys) = unzip ps
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in printPanels' minX maxX minY maxY panels

printPanels' :: Int -> Int -> Int -> Int -> Panels -> String
printPanels' minX maxX minY maxY panels
  = intercalate "\n"
  $ [ printLine minX maxX y panels | y <- [minY..maxY] ]

printLine :: Int -> Int -> Int -> Panels -> String
printLine minX maxX y panels = [ if Set.member (x,y) panels then '#' else '.' | x <- [minX..maxX] ]

initialRobot :: Robot
initialRobot = Robot (0,0) U

runRobot :: Panels -> Memory -> [Panels]
runRobot initialPanels mem =
  let os = exec is mem
      is = map (uncurry colorAtPosition) rs
      rs = scanl moveRobot (initialRobot, initialPanels) (chunksOf 2 os)
  in map snd rs

moveRobot :: (Robot, Panels) -> [Integer] -> (Robot, Panels)
moveRobot (r, ps) [color, turn] =
  let newDirection = doTurn turn (direction r)
      newR = Robot
        { direction = newDirection
        , position = move newDirection (position r)
        }
      newPs = case color of
        0 -> Set.delete (position r) ps
        1 -> Set.insert (position r) ps
        c -> error $ "Unknown color in newPs: " ++ show c
  in (newR, newPs)
moveRobot _ _ = error "Impossible, wrong number of outputs in moveRobot"

colorAtPosition :: Robot -> Panels -> Integer
colorAtPosition r ps =
  case Set.member (position r) ps of
    False -> 0
    True -> 1

doTurn :: Integer -> Direction -> Direction
doTurn 0 U = L
doTurn 0 R = U
doTurn 0 D = R
doTurn 0 L = D
doTurn 1 U = R
doTurn 1 R = D
doTurn 1 D = L
doTurn 1 L = U
doTurn d _ = error $ "Unknown direction in doTurn: " ++ show d

move :: Direction -> Pos -> Pos
move U (x, y) = (x, y - 1)
move R (x, y) = (x + 1, y)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)

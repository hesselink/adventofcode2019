import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import IntCode (memoryFromInputFile, exec, Memory)

main :: IO ()
main = do
  program <- memoryFromInputFile "11"
  print $ Set.size . Set.unions . map (snd . fst3) . runRobot $ program

type Pos = (Int, Int)
type Panels = Set Pos -- not present => black
data Direction = U | R | D | L
  deriving (Show, Eq)
data Robot = Robot
  { position :: Pos
  , direction :: Direction
  } deriving Show

initialRobot :: Robot
initialRobot = Robot (0,0) U

runRobot :: Memory -> [((Robot, Panels), Integer, [Integer])]
runRobot mem =
  let os = exec is mem
      is = map (uncurry colorAtPosition) rs
      rs = scanl moveRobot (initialRobot, Set.empty) (chunksOf 2 os)
  in zip3 rs is (chunksOf 2 os)

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

fst3 :: (x, y, z) -> x
fst3 (x, _, _) = x

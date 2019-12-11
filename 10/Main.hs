import Data.Set (Set, (\\))
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.List (sortBy)

main :: IO ()
main = do
  f <- readFile "input/10"
  let asteroids = parseAsteroids f
      bestPos = maxAsteroidsDetectedFrom asteroids
      numDetected = numAsteroidsDetectedFrom asteroids bestPos
  print bestPos
  print numDetected
  let destroyed = destroyedAsteroidsInOrder asteroids bestPos
      (x, y) = destroyed !! 199
  print (100 * x + y)

type Asteroids = Set Pos
type Pos = (Int, Int)
type Vec = (Int, Int)
type SeenLines = Set Vec -- simplified

maxAsteroidsDetectedFrom :: Asteroids -> Pos
maxAsteroidsDetectedFrom as =
  maximumBy (comparing $ numAsteroidsDetectedFrom as) as

numAsteroidsDetectedFrom :: Asteroids -> Pos -> Int
numAsteroidsDetectedFrom as pos = Set.size $ asteroidsDetectedFrom as pos

asteroidsDetectedFrom :: Asteroids -> Pos -> SeenLines
asteroidsDetectedFrom as pos =
  foldr checkOne Set.empty (Set.toList as)
  where
    checkOne pos2 seen =
      case canSee seen pos pos2 of
        Just v -> Set.insert v seen
        Nothing -> seen

destroyedAsteroidsInOrder :: Asteroids -> Pos -> [Pos]
destroyedAsteroidsInOrder as pos =
  let detectedVecs = asteroidsDetectedFrom as pos
      sortedVecs = sortBy (flip $ comparing angle) (Set.toList detectedVecs)
      sortedAsteroids = map (findAsteroid as pos) sortedVecs
      leftover = deleteAsteroids as pos detectedVecs
  in sortedAsteroids ++ if Set.size leftover == 1 then [] else destroyedAsteroidsInOrder leftover pos

deleteAsteroids :: Asteroids -> Pos -> SeenLines -> Asteroids
deleteAsteroids as pos seen = as \\ Set.map (findAsteroid as pos) seen

findAsteroid :: Asteroids -> Pos -> Vec -> Pos
findAsteroid as p v = head . filter (\a -> a `Set.member` as) . drop 1 . iterate (shift v) $ p

canSee :: SeenLines -> Pos -> Pos -> Maybe Vec
canSee seen origin target =
  let vec = target `vecFrom` origin
      simplified = simplify vec
  in if vec == (0,0) || simplified `Set.member` seen
     then Nothing
     else Just simplified

vecFrom :: Pos -> Pos -> Vec
vecFrom (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

shift :: Vec -> Pos -> Pos
shift (vx, vy) (px, py) = (px + vx, py + vy)

angle :: Vec -> Double
angle (x, y) = atan2 (fromIntegral x) (fromIntegral y)

-- (0,-1) => pi
-- (1,-1) => 3pi/4
-- (1,0) => pi/2
-- (1,1) => pi/4
-- (0,1) => 0
-- (-1,1) => -pi/4
-- (-1,0) => -pi/2
-- (-1,-1) => -3pi/4

parseAsteroids :: String -> Asteroids
parseAsteroids str = Set.fromList
                   . catMaybes
                   . concatMap (\(y,l) ->
                       zipWith (\x c -> if c == '#' then Just (x, y) else Nothing) [0..] l)
                   . zip [0..]
                   . lines
                   $ str

-- simplify (x,y) as a fraction x/y
simplify :: Vec -> Vec
simplify (0,y) = (0,signum y)
simplify (x,0) = (signum x,0)
simplify (x,y) =
  let cd = gcd x y
  in (x `div` cd, y `div` cd)

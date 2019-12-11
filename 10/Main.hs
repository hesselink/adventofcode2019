import Data.Set (Set)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  f <- readFile "input/10"
  let asteroids = parseAsteroids f
  print (maxAsteroidsDetectedFrom asteroids)
  return ()

type Asteroids = Set Pos
type Pos = (Int, Int)
type Vec = (Int, Int)
type SeenLines = Set Vec -- simplified

maxAsteroidsDetectedFrom :: Asteroids -> Int
maxAsteroidsDetectedFrom as =
  let maxPos = maximumBy (comparing $ numAsteroidsDetectedFrom as) as
  in numAsteroidsDetectedFrom as maxPos

numAsteroidsDetectedFrom :: Asteroids -> Pos -> Int
numAsteroidsDetectedFrom as pos = Set.size $ asteroidsDetectedFrom as pos

asteroidsDetectedFrom :: Asteroids -> Pos -> Set Vec
asteroidsDetectedFrom as pos =
  foldr checkOne Set.empty (Set.toList as)
  where
    checkOne pos2 seen =
      case canSee seen pos pos2 of
        Just v -> Set.insert v seen
        Nothing -> seen

canSee :: SeenLines -> Pos -> Pos -> Maybe Vec
canSee seen origin target =
  let vec = target `vecFrom` origin
      simplified = simplify vec
  in if vec == (0,0) || simplified `Set.member` seen
     then Nothing
     else Just simplified

vecFrom :: Pos -> Pos -> Vec
vecFrom (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

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

{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
import Text.ParserCombinators.ReadP (ReadP)
import Data.Maybe (listToMaybe, fromJust)
import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.List (foldl1')

main :: IO ()
main = do
  f <- readFile "input/12"
  let initialMoons = map mkInitialMoon . parsePositions $ f
      iterations = iterate applyGravity initialMoons
  print (sum $ map energy $ iterations !! 1000)
  -- Calculate repititions of x/y/z pos+vel separately, then find the
  -- lowest common denominator to get the total reposition frequency
  let initialX = map (getX . position &&& getX . velocity) initialMoons
      initialY = map (getY . position &&& getY . velocity) initialMoons
      initialZ = map (getZ . position &&& getZ . velocity) initialMoons
      stepsNeededX = length (takeWhile ((/= initialX) . map (getX . position &&& getX . velocity)) (drop 1 iterations)) + 1
      stepsNeededY = length (takeWhile ((/= initialY) . map (getY . position &&& getY . velocity)) (drop 1 iterations)) + 1
      stepsNeededZ = length (takeWhile ((/= initialZ) . map (getZ . position &&& getZ . velocity)) (drop 1 iterations)) + 1
  print (lcds [stepsNeededX, stepsNeededY, stepsNeededZ])


lcd :: Integral a => a -> a -> a
lcd x y = x * y `div` gcd x y

lcds :: Integral a => [a] -> a
lcds = foldl1' lcd

data Triple a = Triple a a a
  deriving (Show, Eq, Ord, Functor, Foldable)

instance Num a => Num (Triple a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Applicative Triple where
  pure x = Triple x x x
  Triple f g h <*> Triple x y z = Triple (f x) (g y) (h z)

type Pos = Triple Int
type Vec = Triple Int
data Moon = Moon
  { position :: Pos
  , velocity :: Vec
  } deriving (Show, Eq)

getX, getY, getZ :: Triple a -> a
getX (Triple x _ _) = x
getY (Triple _ y _) = y
getZ (Triple _ _ z) = z

potentialEnergy :: Moon -> Int
potentialEnergy = sum . abs . position

kineticEnergy :: Moon -> Int
kineticEnergy = sum . abs . velocity

energy :: Moon -> Int
energy m = potentialEnergy m * kineticEnergy m

applyGravity :: [Moon] -> [Moon]
applyGravity = go []
  where
    go _ [] = []
    go doneMs (m:ms) = applyGravity1 m (ms ++ doneMs) : go (m:doneMs) ms

applyGravity1 :: Moon -> [Moon] -> Moon
applyGravity1 m ms =
  let pos = position m
      offsets = sum . map (liftA2 (\x y -> toOffset $ compare x y) pos . position) $ ms
      newVelocity = velocity m + offsets
  in m { position = pos + newVelocity, velocity = newVelocity }

toOffset :: Ordering -> Int
toOffset LT = 1
toOffset EQ = 0
toOffset GT = -1

mkInitialMoon :: Pos -> Moon
mkInitialMoon pos = Moon
  { position = pos
  , velocity = 0
  }

parsePositions :: String -> [Pos]
parsePositions = fromJust . runParser pPositions

pPositions :: Parser [Pos]
pPositions = P.sepBy pPosition (P.char '\n') <* P.skipSpaces

pPosition :: Parser Pos
pPosition = Triple
  <$  P.string "<x="
  <*> pInt
  <*  P.string ", y="
  <*> pInt
  <*  P.string ", z="
  <*> pInt
  <*  P.string ">"


-- TODO refactor into lib
type Parser = ReadP

runParser :: Parser a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . filter ((== "") . snd) . P.readP_to_S p

pInt :: Parser Int
pInt = pRead

pRead :: Read a => Parser a
pRead = P.readS_to_P reads

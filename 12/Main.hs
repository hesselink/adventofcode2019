{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
import Data.Maybe (fromJust)
import qualified Text.ParserCombinators.ReadP as P
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.List (foldl1')

import Parser

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

type Pos = Triple Integer
type Vec = Triple Integer
data Moon = Moon
  { position :: Pos
  , velocity :: Vec
  } deriving (Show, Eq)

getX, getY, getZ :: Triple a -> a
getX (Triple x _ _) = x
getY (Triple _ y _) = y
getZ (Triple _ _ z) = z

potentialEnergy :: Moon -> Integer
potentialEnergy = sum . abs . position

kineticEnergy :: Moon -> Integer
kineticEnergy = sum . abs . velocity

energy :: Moon -> Integer
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

toOffset :: Ordering -> Integer
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

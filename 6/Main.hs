import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  f <- readFile "input"
  let orbits = parseOrbits f
      count = countOrbits orbits
  print count

type Orbits = Map String [String]

countOrbits :: Orbits -> Int
countOrbits os = snd $ go "COM"
  where
    go :: String -> (Int, Int) -- direct + indirect, grand total
    go c =
      let direct = fromMaybe [] $ Map.lookup c os
          previous = map go direct
          directAndIndirect = length direct + sum (map fst previous)
      in (directAndIndirect, directAndIndirect + sum (map snd previous))

--total x = direct x + sum (total (x - 1))
--grand_total x = total x + sum (grand_total (x - 1))

parseOrbits :: String -> Orbits
parseOrbits = foldr (\o -> Map.insertWith (++) (center o) [satellite o]) Map.empty . map parseOrbit . lines

data Orbit = Orbit
  { center :: String
  , satellite :: String
  } deriving Show

parseOrbit :: String -> Orbit
parseOrbit str =
  let (c, s) = break (== ')') str
  in Orbit c (tail s)

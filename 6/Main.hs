{- stack script
   --resolver lts-14.14
   --package hashable
   --package fgl
-}
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.SP as Graph
import Data.Hashable (hash)
import Data.List (nub)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  f <- readFile "input"
  let orbits = parseOrbits f
      count = countOrbits orbits
  print count
  let orbits2 = addRevEdges orbits
      path = Graph.sp (hash "YOU") (hash "SAN") orbits2
  print (length (fromJust path) - 3) -- Subtract 3 because the first two are you and the center you're orbiting, and the last is santa itself.

type Orbits = Gr String Int

addRevEdges :: Orbits -> Orbits
addRevEdges os =
  let edges = Graph.labEdges os
      flipEdge (n1, n2, l) = (n2, n1, l)
  in Graph.insEdges (map flipEdge edges) os

countOrbits :: Orbits -> Int
countOrbits os = snd $ go (hash "COM")
  where
    go :: Int -> (Int, Int) -- direct + indirect, grand total
    go c =
      let direct = Graph.out os c
          previous = map (go . snd3) direct
          directAndIndirect = length direct + sum (map fst previous)
          snd3 (_, y, _) = y
      in (directAndIndirect, directAndIndirect + sum (map snd previous))

--total x = direct x + sum (total (x - 1))
--grand_total x = total x + sum (grand_total (x - 1))

parseOrbits :: String -> Orbits
parseOrbits = orbitsToGraph . map parseOrbit . lines

orbitsToGraph :: [Orbit] -> Orbits
orbitsToGraph os =
  let nodes = map mkNode
            . nub
            $ map center os ++ map satellite os
      edges = map mkEdge os
  in Graph.mkGraph nodes edges

mkNode :: String -> Graph.LNode String
mkNode l = (hash l, l)

-- todo are these directed?
mkEdge :: Orbit -> Graph.LEdge Int
mkEdge o = (hash . center $ o, hash . satellite $ o, 1 {-weight-})

data Orbit = Orbit
  { center :: String
  , satellite :: String
  } deriving Show

parseOrbit :: String -> Orbit
parseOrbit str =
  let (c, s) = break (== ')') str
  in Orbit c (tail s)

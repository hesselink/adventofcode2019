{-# LANGUAGE TupleSections #-}
import Data.Graph.AStar
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Char (isLetter)
import Control.Arrow (second)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/20"
  let byPos = Map.fromList
            . concatMap (\(y, l) -> zipWith (\x c -> ((x, y), c)) [-2..] l)
            . zip [-2..]
            . lines
            $ f
      maxX = subtract 5 {- two letters on each side, plus one for zero index -} . length . head . lines $ f
      maxY = subtract 5 . length . lines $ f
      labels = findLabels (lines f)
      path1 = findPath False byPos labels (maxX, maxY)
      path2 = findPath True byPos labels (maxX, maxY)
  print (length . fromJust $ path1)
  print (length . fromJust $ path2)

findPath :: Bool -> Grid -> Labels -> Pos -> Maybe [Loc]
findPath nestedLevels byPos labels maxPos =
  let goalPos = (0, head . fromJust . Map.lookup "ZZ" $ labels)
  in aStar
       (findAdjacent nestedLevels byPos (labelsToEdges labels) maxPos)
       (\_ _ -> 1)
       (distanceEstimate goalPos)
       (== goalPos)
       (0, head . fromJust . Map.lookup "AA" $ labels)

type Pos = (Int, Int)
type Loc = (Int, Pos) -- level
type Grid = Map Pos Char
type Labels = Map String [Pos]
type LabelEdges = Map Pos Pos

labelsToEdges :: Labels -> LabelEdges
labelsToEdges = Map.fromList . concatMap labelToEdges . filter ((== 2) . length) . Map.elems

labelToEdges :: [Pos] -> [(Pos, Pos)]
labelToEdges [p1,p2] = [(p1,p2), (p2,p1)]
labelToEdges _ = error "Expected 2 positions"

findAdjacent :: Bool -> Grid -> LabelEdges -> Pos -> Loc -> HashSet Loc
findAdjacent nestedLevels gr labels maxPos loc@(lvl, pos) =
  let neighbors = openNeighbors gr pos
      throughPortal = portalConnection nestedLevels loc labels maxPos
  in HashSet.union (HashSet.map (lvl,) neighbors) throughPortal

portalConnection :: Bool -> Loc -> LabelEdges -> Pos -> HashSet Loc
portalConnection nestedLevels (lvl, pos@(x,y)) labels (maxX, maxY) =
  let other = Map.lookup pos labels
      isOuter = x == 0 || y == 0 || x == maxX || y == maxY
      offset = if nestedLevels
               then if isOuter then -1 else 1
               else 0
  in if nestedLevels && lvl == 0 && isOuter
     then HashSet.empty
     else HashSet.fromList $ maybe [] (\p -> [(lvl + offset,p)]) other

openNeighbors :: Grid -> Pos -> HashSet Pos
openNeighbors gr (x,y) = HashSet.fromList
  [ (x', y')
  | (x', y') <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  , let mc = Map.lookup (x',y') gr
  , mc == Just '.'
  ]

distanceEstimate :: Loc -> Loc -> Int
distanceEstimate _ _ = 1 -- abs (x - xg) + abs (y - yg) This doesn't work anymore due to portals

findLabels :: [String] -> Labels
findLabels ls =
  let byPos = concatMap (\(y, l) -> zipWith (\x c -> ((x, y), c)) [-2..] l) . zip [-2..] $ ls
      lbls = mapMaybe (uncurry $ findLabel (Map.fromList byPos)) byPos
  in Map.fromListWith (++) . map (second pure) $ lbls

findLabel :: Grid -> Pos -> Char -> Maybe (String, Pos)
findLabel gr (x,y) c =
  if isLetter c
  then
    let above = fromMaybe ' ' (Map.lookup (x,y-1) gr)
        below = fromMaybe ' ' (Map.lookup (x,y+1) gr)
        left  = fromMaybe ' ' (Map.lookup (x-1,y) gr)
        right = fromMaybe ' ' (Map.lookup (x+1,y) gr)
    in if isLetter above && below == '.'
       then Just ([above,c], (x,y+1))
       else if isLetter below && above == '.'
       then Just ([c,below], (x,y-1))
       else if isLetter left && right == '.'
       then Just ([left,c], (x+1,y))
       else if isLetter right && left == '.'
       then Just ([c,right], (x-1,y))
       else Nothing
  else Nothing

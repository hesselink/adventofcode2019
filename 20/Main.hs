import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, neighbors, lab, insEdges)
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.Query.SP (sp)
import Data.Hashable (hash)
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Char (isLetter)
import Control.Arrow (second)
import qualified Data.Map as Map

main :: IO ()
main = do
  f <- readFile "input/20"
  let graph = buildGraph (lines f)
      labels = findLabels (lines f)
      portalEdges = mkPortals labels
      fullGraph = insEdges portalEdges graph
      path = sp (hash . head . fromJust . Map.lookup "AA" $ labels) (hash . head . fromJust . Map.lookup "ZZ" $ labels) fullGraph
  print (subtract 1 . length . fromJust $ path)

type Pos = (Int, Int)
type Grid = Map Pos Char
type Graph = Gr Pos Double
type Node = LNode Pos
type Edge = LEdge Double
type Labels = Map String [Pos]

buildGraph :: [String] -> Graph
buildGraph ls =
  let byPos = concatMap (\(y, l) -> zipWith (\x c -> ((x, y), c)) [-2..] l) . zip [-2..] $ ls
      (ns, ess) = unzip $ map (uncurry $ mkNodeAndEdges (Map.fromList byPos)) byPos
  in mkGraph ns (concat ess)

mkNodeAndEdges :: Grid -> Pos -> Char -> (Node, [Edge])
mkNodeAndEdges gr p c =
  case c of
    '.' ->
      let ns = openNeighbors gr p
          nId = hash p
          es = map (\np -> (nId, hash np, 1.0)) ns
      in ((nId, p), es)
    _ -> ((hash p, p), []) -- maybe not create anything?

mkPortals :: Labels -> [Edge]
mkPortals = concatMap mkPortal . Map.toList

mkPortal :: (String, [Pos]) -> [Edge]
mkPortal (_, [_]) = []
mkPortal (_, (p1:p2:[])) =
  let id1 = hash p1
      id2 = hash p2
  in [(id1, id2, 1.0), (id2, id1, 1.0)]
mkPortal (_, _) = error "Weird list size in mkPortal"

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

openNeighbors :: Grid -> Pos -> [Pos]
openNeighbors gr (x,y) =
  [ (x', y')
  | (x', y') <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  , let mc = Map.lookup (x',y') gr
  , mc == Just '.'
  ]

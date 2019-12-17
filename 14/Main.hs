import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow (second)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (find)
import qualified Text.ParserCombinators.ReadP as P

import Parser

import Debug.Trace

main :: IO ()
main = do
  f <- readFile "input/14"
  let rs = parseReactions f
  print (oreRequired rs)

type Reactions = Map Chemical Reaction
type Reaction = (Integer, Ingredients)
type Ingredients = [Ingredient]
type Ingredient = (Chemical, Integer)
type Chemical = String

oreRequired :: Reactions -> Integer
oreRequired rs = go (snd . fromJust . Map.lookup "FUEL" $ rs) [] 0
  where
    go [] _ n = n
    go (("ORE", m):is) left n = go is left (n + m)-- TODO use and fill left
    go (i:is) left n = -- TODO use and fill left
      let (prodIs, newLeft) = findIngredientsFor i left rs
          mOre = find ((== "ORE") . fst) prodIs
      in traceShow (fst i, "needs", prodIs) $ go (is ++ filter ((/= "ORE") . fst) prodIs) newLeft (n + maybe 0 snd mOre)

findIngredientsFor :: Ingredient -> [Ingredient] -> Reactions -> (Ingredients, Ingredients)
findIngredientsFor (i, n) left rs =
  let (m, is) = fromJust $ Map.lookup i rs
      (needed, newLeft) = removeIngredients is left
      timesNeeded = n `div` m + signum (n `mod` m)
  in (map (second (* timesNeeded)) needed, newLeft) -- TODO add to leftovers

removeIngredients :: Ingredients -> Ingredients -> (Ingredients, Ingredients)
removeIngredients needed left = _

removeIngredient :: Ingredient -> Ingredients -> (Ingredient, Ingredients)
removeIngredient i@(ch, n) left =
  let lMap = Map.fromList left
      mIn = Map.lookup ch left
  in maybe (i, left) (\m -> Map.update (\v -> if v > m mIn

parseReactions :: String -> Reactions
parseReactions = fromJust . runParser pReactions

pReactions :: Parser Reactions
pReactions = Map.fromList <$> P.sepBy1 pReaction (P.char '\n') <* P.skipSpaces

pReaction :: Parser (Chemical, Reaction)
pReaction = (\is (ch, n) -> (ch, (n, is)))
  <$> P.sepBy1 pIngredient (P.string ", ")
  <*  P.skipSpaces
  <*  P.string "=>"
  <*  P.skipSpaces
  <*> pIngredient

pIngredient :: Parser Ingredient
pIngredient = (flip (,) <$> pInt <* P.skipSpaces <*> pId)

pId :: Parser String
pId = P.munch1 (`elem` ['A'..'Z'])

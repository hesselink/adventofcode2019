import Data.Char (digitToInt)
main :: IO ()
main = do
  f <- readFile "input/16"
  let digits = map digitToInt . head . lines $ f
      phase100 = getPhase 100 digits
  putStrLn . concatMap show . take 8 $ phase100

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

pattern :: Int -> [Int]
pattern n = allPatterns !! n
  where allPatterns = map (\i -> drop 1 . cycle . concatMap (replicate (i+1)) $ basePattern) [0..]

digit :: [Int] -> Int -> Int
digit signal d = (`mod` 10) . abs . sum . zipWith (*) (pattern d) $ signal

next :: [Int] -> [Int]
next signal = map (digit signal) [0..length signal - 1]

getPhase :: Int -> [Int] -> [Int]
getPhase n digits =
  let phases = iterate next digits
  in phases !! n

-- nth digit of the result doesn't depend on the first n digits of the
-- input. Input is 650, *10000 is 65.000.000. But the first 8 digits
-- are 59758034 so we can skip the first ~60M. This still leaves 5.241.966 digits.
-- Currently this is quadratic in the number of digits, because we map
-- 'digit' over the signal, and then digit zips and sums the signal
-- again. We need some kind of dynamic programming approach perhaps?
--
-- The 5M inputs repeat every 650 items. The input patterns repeat
-- every 4/8/12/16 etc items.
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

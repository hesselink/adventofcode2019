module Main (main, printBeam) where

import IntCode
import Data.List.Split (chunksOf)
import Data.List (group, unfoldr)

main :: IO ()
main = do
  program <- memoryFromInputFile "19"
  let size = sum . map (uncurry $ flip (-)) . take 50 . traceBeam $ program
  print size
  let (x, y) = findSquareFit program 100
  print (10000 * x + y)

findSquareFit :: Memory -> Int -> (Int, Int)
findSquareFit program n =
  let interesting = dropWhile ((\(x1,x2) -> x2 - x1 < n) . snd) . zip [0..] . traceBeam $ program
      zipped = zip interesting (drop (n - 1) interesting)
      good = filter (uncurry (fit n)) zipped
      ((y, (_, xmax)), _) = head good
  in (xmax - n, y)

fit :: Int -> (Int, (Int, Int)) -> (Int, (Int, Int)) -> Bool
-- fit n p1@(y, (x1,x2)) p2@(y', (x1',x2')) | y `mod` 100 == 0 && traceShow (p1, p2) False = undefined
fit n (_, (_,x2)) (_, (x1',_)) = x2 - x1' >= n

traceBeam :: Memory -> [(Int, Int)]
traceBeam = traceBeam' (0,0) [0..]

traceBeam' :: (Int, Int) -> [Int] -> Memory -> [(Int, Int)]
traceBeam' (xmin, xmax) allYs program = unfoldr nextItem ((xmin, xmax), allYs)
  where
    nextItem ((prevXMin, prevXMax), y:ys) =
      let r = beamEdges program prevXMin prevXMax y
      in Just (r, (r, ys))
    nextItem (_, []) = Nothing -- Shouldn't happen, we always pass infinite ys

beamEdges :: Memory -> Int -> Int -> Int -> (Int, Int)
beamEdges program prevXMin prevXMax y =
  let is = [ [x, fromIntegral y] | x <- [fromIntegral prevXMin..fromIntegral prevXMax + 10] ] -- 10 is totally arbitrary
      os = concatMap (flip exec program) is
      (g1@(c:_):rs@_) = group os
      startsEmpty = c == 0
  in if startsEmpty
     then if null rs then (prevXMin, prevXMin) else (prevXMin + length g1, prevXMin + length g1 + length (head rs))
     else (prevXMin, prevXMin + length g1)

printBeam :: [Int] -> IO ()
printBeam = mapM_ putStrLn
          . chunksOf 50
          . map (\x -> if x == 0 then '.' else '#')

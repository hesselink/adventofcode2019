import IntCode
import Data.List.Split

main :: IO ()
main = do
  f <- readFile "input/9"
  let program = map read . splitOn "," $ f
      output = exec [1] program
  print output
  let output2 = exec [2] program
  print output2

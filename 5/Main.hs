import Data.List.Split (splitOn)

import IntCode

main :: IO ()
main = do
  f <- readFile "input/5"
  let initialMem = map read . splitOn "," $ f
      out = exec [1] initialMem
  print (last out)
  let out2 = exec [5] initialMem
  print (last out2)

import Data.List.Split (splitOn)

import IntCode

main :: IO ()
main = do
  f <- readFile "input/5"
  let initialMem = map read . splitOn "," $ f
      (_, out) = run [1] initialMem
  print (last out)
  let (_, out2) = run [5] initialMem
  print (last out2)
  return ()

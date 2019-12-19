import IntCode

main :: IO ()
main = do
  program <- memoryFromInputFile "19"
  let os = concatMap (flip exec program) [ [x, y] | x <- [0..49], y <- [0..49] ]
  print (sum os)

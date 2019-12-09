import IntCode

main :: IO ()
main = do
  initialMem <- memoryFromInputFile "5"
  let out = exec [1] initialMem
  print (last out)
  let out2 = exec [5] initialMem
  print (last out2)

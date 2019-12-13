import IntCode

main :: IO ()
main = do
  program <- memoryFromInputFile "9"
  let out = exec [1] program
  print out
  let out2 = exec [2] program
  print out2

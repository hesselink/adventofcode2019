import IntCode

main :: IO ()
main = do
  program <- memoryFromInputFile "9"
  let output = exec [1] program
  print output
  let output2 = exec [2] program
  print output2

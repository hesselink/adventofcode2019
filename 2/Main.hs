import IntCode

main :: IO ()
main = do
  numbers <- memoryFromInputFile "2"
  let result = run [] . restoreGravityAssist $ numbers
  print (head . fst $ result)
  let inputs = [ (n, v) | n <- [0..99], v <- [0..99] ]
      (rN, rV) = head [ (n, v) | (n, v) <- inputs, (head . fst . run [] . setInputs n v $ numbers) == 19690720 ]
  print (100 * rN + rV)

restoreGravityAssist :: Memory -> Memory
restoreGravityAssist = setInputs 12 2

setInputs :: Integer -> Integer -> Memory -> Memory
setInputs noun verb = setAt (Address 1) noun . setAt (Address 2) verb

import Data.List (permutations)

import IntCode

main :: IO ()
main = do
  initialMem <- memoryFromInputFile "7"
  let maxOutput = findMaxOutput initialMem
  print maxOutput
  let maxOutput2 = findMaxOutputFeedback initialMem
  print maxOutput2

type Phase = Integer
type Input = Integer
type Output = Integer

findMaxOutputFeedback :: Memory -> Output
findMaxOutputFeedback mem = maximum . map (runAmplifiersFeedback mem) $ permutations [5,6,7,8,9]

findMaxOutput :: Memory -> Output
findMaxOutput mem = maximum . map (runAmplifiers mem) $ permutations [0,1,2,3,4]

runAmplifiersFeedback :: Memory -> [Phase] -> Output
runAmplifiersFeedback mem phases =
  let result = foldr (\ph os -> exec (ph:os) mem) (0:result) phases
  in last result

runAmplifiers :: Memory -> [Phase] -> Output
runAmplifiers mem = foldr (\ph i -> runAmplifier ph i mem) 0

runAmplifier :: Phase -> Input -> Memory -> Output
runAmplifier phase inp = head . snd . run [phase, inp]

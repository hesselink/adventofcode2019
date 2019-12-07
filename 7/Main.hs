import Data.List (permutations)
import Data.List.Split (splitOn)

import IntCode

main :: IO ()
main = do
  f <- readFile "input/7"
  let initialMem = map read . splitOn "," $ f
      maxOutput = findMaxOutput initialMem
  print maxOutput
  let maxOutput2 = findMaxOutputFeedback initialMem
  print maxOutput2

type Phase = Int
type Input = Int
type Output = Int

findMaxOutputFeedback :: Memory -> Output
findMaxOutputFeedback mem = maximum . map (runAmplifiersFeedback mem) $ permutations [5,6,7,8,9]

findMaxOutput :: Memory -> Output
findMaxOutput mem = maximum . map (runAmplifiers mem) $ permutations [0,1,2,3,4]

runAmplifiersFeedback :: Memory -> [Phase] -> Output
runAmplifiersFeedback mem [p1,p2,p3,p4,p5] =
  let (_, os1) = runLabeled "1" (p1:0:os5) mem
      (_, os2) = runLabeled "2" (p2:os1) mem
      (_, os3) = runLabeled "3" (p3:os2) mem
      (_, os4) = runLabeled "4" (p4:os3) mem
      (_, os5) = runLabeled "5" (p5:os4) mem
  in last os5
runAmplifiersFeedback _ _ = error "Pass 5 phases"

runAmplifiers :: Memory -> [Phase] -> Output
runAmplifiers mem = foldr (\ph i -> runAmplifier ph i mem) 0

runAmplifier :: Phase -> Input -> Memory -> Output
runAmplifier phase input = head . snd . run [phase, input]

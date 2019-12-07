import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f <- readFile "input/2"
  let numbers = map read . splitOn "," $ f
      result = run . restoreGravityAssist $ numbers
  print (head result)
  let inputs = [ (n, v) | n <- [0..99], v <- [0..99] ]
      (rN, rV) = head [ (n, v) | (n, v) <- inputs, (head . run . setInputs n v $ numbers) == 19690720 ]
  print (100 * rN + rV)

data Instr
  = Add Address Address Address
  | Mul Address Address Address
  | Halt
  deriving Show

type Address = Int
type Memory = [Int]

data InterpreterState = InterpreterState
  { memory :: Memory
  , position :: Int
  , done :: Bool
  } deriving Show

type Interpreter = State InterpreterState

restoreGravityAssist :: Memory -> Memory
restoreGravityAssist = setInputs 12 2

setInputs :: Int -> Int -> Memory -> Memory
setInputs noun verb = setAt 1 noun . setAt 2 verb

run :: Memory -> Memory
run vs = evalState run' (InterpreterState vs 0 False)

run' :: Interpreter Memory
run' = do
  step
  d <- gets done
  if d then gets memory else run'

step :: Interpreter ()
step = do
  st <- get
  let vs = drop (position st) (memory st)
      op = fromJust $ parseInstr vs
      mNewVs = runInstr op (memory st)
  case mNewVs of
    Nothing -> put st { done = True }
    Just newVs -> put st { memory = newVs, position = position st + sizeOf op }


parseInstr :: Memory -> Maybe Instr
parseInstr (1:x:y:z:_) = Just (Add x y z)
parseInstr (2:x:y:z:_) = Just (Mul x y z)
parseInstr (99:_) = Just Halt
parseInstr _ = Nothing

sizeOf :: Instr -> Int
sizeOf _q@Add{} = 4
sizeOf _q@Mul{} = 4
sizeOf _q@Halt{} = 1

runInstr :: Instr -> Memory -> Maybe Memory
runInstr (Add x y z) inputs =
  let v1 = inputs !! x
      v2 = inputs !! y
  in Just $ setAt z (v1 + v2) inputs
runInstr (Mul x y z) inputs =
  let v1 = inputs !! x
      v2 = inputs !! y
  in Just $ setAt z (v1 * v2) inputs
runInstr Halt _ = Nothing

setAt :: Int -> x -> [x] -> [x]
setAt _ _ [] = []
setAt 0 v (_:xs) = v:xs
setAt n v (x:xs) = x : setAt (n-1) v xs

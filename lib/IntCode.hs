module IntCode where

import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, listToMaybe)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P

data Instr
  = Add Param Param Address
  | Mul Param Param Address
  | Input Address
  | Output Param
  | JumpIfTrue Param Param
  | JumpIfFalse Param Param
  | LessThan Param Param Address
  | Equal Param Param Address
  | Halt
  deriving Show

data OpCode
  = OpAdd
  | OpMul
  | OpInput
  | OpOutput
  | OpJumpIfTrue
  | OpJumpIfFalse
  | OpLessThan
  | OpEqual
  | OpHalt
  deriving (Show, Eq)

data Param
  = PAddress Address | PValue Value
  deriving Show

data ParamMode
  = PMAddress
  | PMValue
  deriving (Show, Eq)

newtype Address = Address Int deriving Show
newtype Value = Value Int deriving Show

type Memory = [Int]

data InterpreterState = InterpreterState
  { memory :: Memory
  , position :: Int
  , inputs :: [Int]
  , outputs :: [Int]
  , done :: Bool
  , label :: String -- for debugging
  } deriving Show

type Interpreter = State InterpreterState

execLabeled :: String -> [Int] -> Memory -> [Int]
execLabeled l is = snd . runLabeled l is

exec :: [Int] -> Memory -> [Int]
exec = execLabeled "<no label>"

runLabeled :: String -> [Int] -> Memory -> (Memory, [Int])
runLabeled l is vs = evalState run' (InterpreterState vs 0 is [] False l)

run :: [Int] -> Memory -> (Memory, [Int])
run = runLabeled "<no label>"

run' :: Interpreter (Memory, [Int])
run' = do
  step
  d <- gets done
  if d then gets (\s -> (memory s, reverse $ outputs s)) else run'

step :: Interpreter ()
step = do
  st <- get
  let vs = drop (position st) (memory st)
      op = fromJust $ parseInstr vs
  runInstr op

parseInstr :: Memory -> Maybe Instr
parseInstr mem = runParser pInstr (intercalate "," . map show $ mem)

type Parser = ReadP

runParser :: Parser a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . P.readP_to_S p

pInstr :: Parser Instr
pInstr =  pAdd <|> pMul <|> pInput <|> pOutput <|> pJumpIfTrue <|> pJumpIfFalse <|> pLessThan
      <|> pEquals <|> pHalt

pAdd :: Parser Instr
pAdd = pThreeParamInstr OpAdd Add

pMul :: Parser Instr
pMul = pThreeParamInstr OpMul Mul

pLessThan :: Parser Instr
pLessThan = pThreeParamInstr OpLessThan LessThan

pEquals :: Parser Instr
pEquals = pThreeParamInstr OpEqual Equal

pJumpIfFalse :: Parser Instr
pJumpIfFalse = pTwoParamInstr OpJumpIfFalse JumpIfFalse

pJumpIfTrue :: Parser Instr
pJumpIfTrue = pTwoParamInstr OpJumpIfTrue JumpIfTrue

pInput :: Parser Instr
pInput = do
  void $ guardInstr OpInput
  pComma
  p1 <- pInt
  return $ Input (Address p1)

pOutput :: Parser Instr
pOutput = do
  (m:_) <- guardInstr OpOutput
  pComma
  p1 <- pInt
  return $ Output (mkParam m p1)

pThreeParamInstr :: OpCode -> (Param -> Param -> Address -> Instr) -> Parser Instr
pThreeParamInstr opCode constr = do
  (m1:m2:_) <- guardInstr opCode
  pComma
  p1 <- pInt
  pComma
  p2 <- pInt
  pComma
  p3 <- pInt
  return $ constr (mkParam m1 p1) (mkParam m2 p2) (Address p3)

pTwoParamInstr :: OpCode -> (Param -> Param -> Instr) -> Parser Instr
pTwoParamInstr opCode constr = do
  (m1:m2:_) <- guardInstr opCode
  pComma
  p1 <- pInt
  pComma
  p2 <- pInt
  return $ constr (mkParam m1 p1) (mkParam m2 p2)

guardInstr :: OpCode -> Parser [ParamMode]
guardInstr wantedOpCode = do
  (revParamModes, opCode) <- pInstrHeader
  let paramModes = reverse revParamModes ++ repeat PMAddress
  guard (opCode == wantedOpCode)
  return paramModes

pHalt :: Parser Instr
pHalt = Halt <$ P.string "99"

pInstrHeader :: Parser ([ParamMode], OpCode)
pInstrHeader = (,) <$> P.many pParamMode <*> pOpCode

pParamMode :: Parser ParamMode
pParamMode =  PMAddress <$ P.char '0'
          <|> PMValue   <$ P.char '1'

pOpCode :: Parser OpCode
pOpCode =  OpAdd         <$ P.optional (P.char '0') <* P.char '1'
       <|> OpMul         <$ P.optional (P.char '0') <* P.char '2'
       <|> OpInput       <$ P.optional (P.char '0') <* P.char '3'
       <|> OpOutput      <$ P.optional (P.char '0') <* P.char '4'
       <|> OpJumpIfTrue  <$ P.optional (P.char '0') <* P.char '5'
       <|> OpJumpIfFalse <$ P.optional (P.char '0') <* P.char '6'
       <|> OpLessThan    <$ P.optional (P.char '0') <* P.char '7'
       <|> OpEqual       <$ P.optional (P.char '0') <* P.char '8'
       <|> OpHalt        <$ P.string "99"

pInt :: Parser Int
pInt = pRead

pComma :: Parser ()
pComma = void $ P.char ','

pRead :: Read a => Parser a
pRead = P.readS_to_P reads

mkParam :: ParamMode -> Int -> Param
mkParam PMAddress = PAddress . Address
mkParam PMValue = PValue . Value

sizeOf :: Instr -> Int
sizeOf _q@Add{} = 4
sizeOf _q@Mul{} = 4
sizeOf _q@Input{} = 2
sizeOf _q@Output{} = 2
sizeOf _q@JumpIfTrue{} = 3
sizeOf _q@JumpIfFalse{} = 3
sizeOf _q@LessThan{} = 4
sizeOf _q@Equal{} = 4
sizeOf _q@Halt{} = 1

runInstr :: Instr -> Interpreter ()
runInstr op = do
  st <- get
  let mem = memory st
      nextPosition = position st + sizeOf op
  case op of
    (Add x y z) -> do
      let v1 = resolveParam x mem
          v2 = resolveParam y mem
      put st { memory = setAt z (v1 + v2) mem, position = nextPosition }
    (Mul x y z) -> do
      let v1 = resolveParam x mem
          v2 = resolveParam y mem
      put st { memory = setAt z (v1 * v2) mem, position = nextPosition }
    (Input x) -> do
      let (i:is) = inputs st
      put st { memory = setAt x i mem, inputs = is, position = nextPosition }
    (Output x) -> do
      let v = resolveParam x mem
      put st { outputs = v : outputs st, position = nextPosition }
    (JumpIfTrue p t) -> do
      let v1 = resolveParam p mem
          v2 = resolveParam t mem
      if v1 /= 0
      then put st { position = v2 }
      else put st { position = nextPosition }
    (JumpIfFalse p t) -> do
      let v1 = resolveParam p mem
          v2 = resolveParam t mem
      if v1 == 0
      then put st { position = v2 }
      else put st { position = nextPosition }
    (LessThan x y z) -> do
      let v1 = resolveParam x mem
          v2 = resolveParam y mem
      put st { memory = setAt z (if v1 < v2 then 1 else 0) mem, position = nextPosition }
    (Equal x y z) -> do
      let v1 = resolveParam x mem
          v2 = resolveParam y mem
      put st { memory = setAt z (if v1 == v2 then 1 else 0) mem, position = nextPosition }
    Halt -> put st { done = True }

resolveParam :: Param -> Memory -> Int
resolveParam (PAddress (Address x)) mem = mem !! x
resolveParam (PValue (Value v)) _ = v

setAt :: Address -> x -> [x] -> [x]
setAt _ _ [] = []
setAt (Address 0) v (_:xs) = v:xs
setAt (Address n) v (x:xs) = x : setAt (Address $ n-1) v xs

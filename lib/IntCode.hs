module IntCode where

import Control.Monad.State
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, listToMaybe)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P

data Instr
  = Add Param Param OutParam
  | Mul Param Param OutParam
  | Input OutParam
  | Output Param
  | JumpIfTrue Param Param
  | JumpIfFalse Param Param
  | LessThan Param Param OutParam
  | Equal Param Param OutParam
  | AdjustRelativeBase Param
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
  | OpAdjustRelativeBase
  deriving (Show, Eq)

data Param
  = PAddress Address | PValue Value | PRelative Offset
  deriving Show

data OutParam
  = OAddress Address | ORelative Offset
  deriving Show

data ParamMode
  = PMAddress
  | PMValue
  | PMRelative
  deriving (Show, Eq)

newtype Address = Address Int deriving Show
newtype Value = Value Integer deriving Show
newtype Offset = Offset Int deriving Show

type Memory = [Integer]

data InterpreterState = InterpreterState
  { memory :: Memory
  , position :: Int
  , inputs :: [Integer]
  , outputs :: [Integer]
  , done :: Bool
  , label :: String -- for debugging
  , relativeBase :: Address
  } deriving Show

type Interpreter = State InterpreterState

execLabeled :: String -> [Integer] -> Memory -> [Integer]
execLabeled l is = snd . runLabeled l is

exec :: [Integer] -> Memory -> [Integer]
exec = execLabeled "<no label>"

runLabeled :: String -> [Integer] -> Memory -> (Memory, [Integer])
runLabeled l is vs = evalState run' InterpreterState
  { memory = vs ++ repeat 0
  , position = 0
  , inputs = is
  , outputs = []
  , done = False
  , label = l
  , relativeBase = Address 0
  }

run :: [Integer] -> Memory -> (Memory, [Integer])
run = runLabeled "<no label>"

run' :: Interpreter (Memory, [Integer])
run' = do
  step
  d <- gets done
  if d then gets (\s -> (memory s, outputs s)) else run'

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
      <|> pEquals <|> pAdjustRelativeBase <|> pHalt

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
  (m:_) <- guardInstr OpInput
  pComma
  p1 <- pInt
  return $ Input (mkOutParam m p1)

pOutput :: Parser Instr
pOutput = do
  (m:_) <- guardInstr OpOutput
  pComma
  p1 <- pInt
  return $ Output (mkParam m p1)

pAdjustRelativeBase :: Parser Instr
pAdjustRelativeBase = do
  (m:_) <- guardInstr OpAdjustRelativeBase
  pComma
  p1 <- pInt
  return $ AdjustRelativeBase (mkParam m p1)

pThreeParamInstr :: OpCode -> (Param -> Param -> OutParam -> Instr) -> Parser Instr
pThreeParamInstr opCode constr = do
  (m1:m2:m3:_) <- guardInstr opCode
  pComma
  p1 <- pInt
  pComma
  p2 <- pInt
  pComma
  p3 <- pInt
  return $ constr (mkParam m1 p1) (mkParam m2 p2) (mkOutParam m3 p3)

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
pParamMode =  PMAddress  <$ P.char '0'
          <|> PMValue    <$ P.char '1'
          <|> PMRelative <$ P.char '2'

pOpCode :: Parser OpCode
pOpCode =  OpAdd                <$ P.optional (P.char '0') <* P.char '1'
       <|> OpMul                <$ P.optional (P.char '0') <* P.char '2'
       <|> OpInput              <$ P.optional (P.char '0') <* P.char '3'
       <|> OpOutput             <$ P.optional (P.char '0') <* P.char '4'
       <|> OpJumpIfTrue         <$ P.optional (P.char '0') <* P.char '5'
       <|> OpJumpIfFalse        <$ P.optional (P.char '0') <* P.char '6'
       <|> OpLessThan           <$ P.optional (P.char '0') <* P.char '7'
       <|> OpEqual              <$ P.optional (P.char '0') <* P.char '8'
       <|> OpAdjustRelativeBase <$ P.optional (P.char '0') <* P.char '9'
       <|> OpHalt               <$ P.string "99"

pInt :: Parser Integer
pInt = pRead

pComma :: Parser ()
pComma = void $ P.char ','

pRead :: Read a => Parser a
pRead = P.readS_to_P reads

mkParam :: ParamMode -> Integer -> Param
mkParam PMAddress = PAddress . Address . fromIntegral
mkParam PMValue = PValue . Value
mkParam PMRelative = PRelative . Offset . fromIntegral

mkOutParam :: ParamMode -> Integer -> OutParam
mkOutParam PMAddress = OAddress . Address . fromIntegral
mkOutParam PMValue = error "Out param with value mode"
mkOutParam PMRelative = ORelative . Offset . fromIntegral

sizeOf :: Instr -> Int
sizeOf _q@Add{} = 4
sizeOf _q@Mul{} = 4
sizeOf _q@Input{} = 2
sizeOf _q@Output{} = 2
sizeOf _q@JumpIfTrue{} = 3
sizeOf _q@JumpIfFalse{} = 3
sizeOf _q@LessThan{} = 4
sizeOf _q@Equal{} = 4
sizeOf _q@AdjustRelativeBase{} = 2
sizeOf _q@Halt{} = 1

runInstr :: Instr -> Interpreter ()
runInstr op = do
  st <- get
  let mem = memory st
      nextPosition = position st + sizeOf op
  case op of
    (Add x y z) -> do
      v1 <- resolveParam x
      v2 <- resolveParam y
      v3 <- resolveOutParam z
      put st { memory = setAt v3 (v1 + v2) mem, position = nextPosition }
    (Mul x y z) -> do
      v1 <- resolveParam x
      v2 <- resolveParam y
      v3 <- resolveOutParam z
      put st { memory = setAt v3 (v1 * v2) mem, position = nextPosition }
    (Input x) -> do
      let (i:is) = inputs st
      v <- resolveOutParam x
      put st { memory = setAt v i mem, inputs = is, position = nextPosition }
    (Output x) -> do
      v <- resolveParam x
      put st { outputs = outputs st ++ [v], position = nextPosition }
    (JumpIfTrue p t) -> do
      v1 <- resolveParam p
      v2 <- resolveParam t
      if v1 /= 0
      then put st { position = fromIntegral v2 }
      else put st { position = nextPosition }
    (JumpIfFalse p t) -> do
      v1 <- resolveParam p
      v2 <- resolveParam t
      if v1 == 0
      then put st { position = fromIntegral v2 }
      else put st { position = nextPosition }
    (LessThan x y z) -> do
      v1 <- resolveParam x
      v2 <- resolveParam y
      v3 <- resolveOutParam z
      put st { memory = setAt v3 (if v1 < v2 then 1 else 0) mem, position = nextPosition }
    (Equal x y z) -> do
      v1 <- resolveParam x
      v2 <- resolveParam y
      v3 <- resolveOutParam z
      put st { memory = setAt v3 (if v1 == v2 then 1 else 0) mem, position = nextPosition }
    (AdjustRelativeBase x) -> do
      v <- resolveParam x
      put st { relativeBase = relativeBase st `offset` (Offset . fromIntegral $ v), position = nextPosition }
    Halt -> put st { done = True }

resolveParam :: Param -> Interpreter Integer
resolveParam (PAddress (Address x)) = do
  mem <- gets memory
  return $ mem !! fromIntegral x -- safe, memory is infinite
resolveParam (PValue (Value v)) = return v
resolveParam (PRelative o) = do
  base <- gets relativeBase
  resolveParam (PAddress (base `offset` o))

resolveOutParam :: OutParam -> Interpreter Address
resolveOutParam (OAddress a) = return a
resolveOutParam (ORelative o) = do
  base <- gets relativeBase
  return (base `offset` o)

offset :: Address -> Offset -> Address
offset (Address a) (Offset o) = Address (a + o)

setAt :: Address -> x -> [x] -> [x]
setAt _ _ [] = []
setAt (Address 0) v (_:xs) = v:xs
setAt (Address n) v (x:xs) = x : setAt (Address $ n-1) v xs

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IntCode where

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import Text.ParserCombinators.ReadP (ReadP)
import Data.Map (Map)
import Data.List.Split (splitOn)
import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Map as Map
import Data.DList (DList, snoc)
import GHC.Exts (IsList(toList))
import Control.Arrow (second)

data Instr = Instr OpCode [ParamMode]
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

newtype Address = Address Int deriving (Show, Eq, Ord)
newtype Value = Value Integer deriving Show
newtype Offset = Offset Int deriving Show

type Memory = Map Address Integer

data InterpreterState = InterpreterState
  { memory :: Memory
  , position :: Address
  , done :: Bool
  , label :: String -- for debugging
  , relativeBase :: Address
  } deriving Show

type Interpreter m = StateT InterpreterState m

class Monad m => MonadIntCode m where
  input :: m Integer
  output :: Integer -> m ()

instance MonadIntCode m => MonadIntCode (StateT s m) where
  input = lift input
  output = lift . output

parseMemory :: String -> Memory
parseMemory = Map.fromAscList . zip (map Address [0..]) . map read . splitOn ","

memoryFromInputFile :: String -> IO Memory
memoryFromInputFile fileName = parseMemory <$> readFile ("input/" ++ fileName)

execLabeled :: String -> [Integer] -> Memory -> [Integer]
execLabeled l is = snd . runLabeled l is

exec :: [Integer] -> Memory -> [Integer]
exec = execLabeled "<no label>"

data IOState = IOState
  { inputs :: [Integer]
  , outputs :: DList Integer
  }

newtype PureMonadIntCode a = PureMonadIntCode { unPureMonadIntCode :: State IOState a }
  deriving (Functor, Applicative, Monad, MonadState IOState)

instance MonadIntCode PureMonadIntCode where
  input = do
    st <- get
    let (i:is) = inputs st
    put st { inputs = is }
    return i
  output o = modify (\st -> st { outputs = outputs st `snoc` o })

runLabeled :: String -> [Integer] -> Memory -> (Memory, [Integer])
runLabeled l is mem = second (toList . outputs) $
  runState (unPureMonadIntCode $ runLabeledM l mem) IOState
    { inputs = is
    , outputs = mempty
    }

data IOActions m = IOActions
  { inputAction :: m Integer
  , outputAction :: Integer -> m ()
  }

newtype ActionMonadIntCode m a = ActionMonadIntCode { unActionMonadIntCode :: ReaderT (IOActions m) m a }
  deriving (Functor, Applicative, Monad, MonadReader (IOActions m))

instance MonadTrans ActionMonadIntCode where
  lift = ActionMonadIntCode . lift

instance Monad m => MonadIntCode (ActionMonadIntCode m) where
  input = do
    act <- asks inputAction
    lift act
  output o = do
    act <- asks outputAction
    lift (act o)

runActions :: Monad m => m Integer -> (Integer -> m ()) -> Memory -> m Memory
runActions = runLabeledActions "<no label>"

runLabeledActions :: Monad m => String -> m Integer -> (Integer -> m ()) -> Memory -> m Memory
runLabeledActions l inp outp mem = runReaderT (unActionMonadIntCode $ runLabeledM l mem) IOActions
  { inputAction = inp
  , outputAction = outp
  }

runLabeledM :: MonadIntCode m => String -> Memory -> m Memory
runLabeledM l mem = evalStateT run' InterpreterState
  { memory = mem
  , position = Address 0
  , done = False
  , label = l
  , relativeBase = Address 0
  }

run :: [Integer] -> Memory -> (Memory, [Integer])
run = runLabeled "<no label>"

run' :: MonadIntCode m => Interpreter m Memory
run' = do
  step
  d <- gets done
  if d then gets memory else run'

step :: MonadIntCode m => Interpreter m ()
step = do
  instr <- readInstr
  runInstr instr

readInstr :: Monad m => Interpreter m Instr
readInstr = do
  st <- get
  let instrStr = show $ readAt (position st) (memory st)
      instr = runParser pInstr instrStr
  modify $ \s -> s { position = position st `offset` Offset 1 }
  return (fromJust instr)

type Parser = ReadP

runParser :: Parser a -> String -> Maybe a
runParser p = fmap fst . listToMaybe . filter ((== "") . snd) . P.readP_to_S p

pInstr :: Parser Instr
pInstr = (\ms cd -> Instr cd (addDefaultModes cd ms)) <$> P.many pParamMode <*> pOpCode
  where
    addDefaultModes cd ms = reverse ms ++ drop (length ms) (replicate (numParams cd) PMAddress)

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

numParams :: OpCode -> Int
numParams OpAdd = 3
numParams OpMul = 3
numParams OpInput = 1
numParams OpOutput = 1
numParams OpJumpIfTrue = 2
numParams OpJumpIfFalse = 2
numParams OpLessThan = 3
numParams OpEqual = 3
numParams OpAdjustRelativeBase = 1
numParams OpHalt = 0

runInstr :: MonadIntCode m => Instr -> Interpreter m ()
runInstr (Instr op modes) = do
  st <- get
  let mem = memory st
  case op of
    OpAdd -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      v3 <- resolveOutParam (modes !! 2)
      modify $ \s -> s { memory = setAt v3 (v1 + v2) mem }
    OpMul -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      v3 <- resolveOutParam (modes !! 2)
      modify $ \s -> s { memory = setAt v3 (v1 * v2) mem }
    OpInput -> do
      i <- input
      v <- resolveOutParam (modes !! 0)
      modify $ \s -> s { memory = setAt v i mem }
    OpOutput -> do
      v <- resolveParam (modes !! 0)
      output v
    OpJumpIfTrue -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      when (v1 /= 0) $ modify $ \s -> s { position = Address (fromIntegral v2) }
    OpJumpIfFalse -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      when (v1 == 0) $ modify $ \s -> s { position = Address (fromIntegral v2) }
    OpLessThan -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      v3 <- resolveOutParam (modes !! 2)
      modify $ \s -> s { memory = setAt v3 (if v1 < v2 then 1 else 0) mem }
    OpEqual -> do
      v1 <- resolveParam (modes !! 0)
      v2 <- resolveParam (modes !! 1)
      v3 <- resolveOutParam (modes !! 2)
      modify $ \s -> s { memory = setAt v3 (if v1 == v2 then 1 else 0) mem }
    OpAdjustRelativeBase -> do
      v <- resolveParam (modes !! 0)
      modify $ \s -> s { relativeBase = relativeBase s `offset` (Offset . fromIntegral $ v) }
    OpHalt -> modify $ \s -> s { done = True }

resolveParam :: Monad m => ParamMode -> Interpreter m Integer
resolveParam PMAddress = withNextValue $ \a -> do
  mem <- gets memory
  return $ readAt (Address . fromIntegral $ a) mem
resolveParam PMValue = withNextValue return
resolveParam PMRelative = withNextValue $ \o -> do
  st <- get
  let mem = memory st
      base = relativeBase st
  return $ readAt (base `offset` Offset (fromIntegral o)) mem

resolveOutParam :: Monad m => ParamMode -> Interpreter m Address
resolveOutParam PMAddress = withNextValue $ \a -> return (Address . fromIntegral $ a)
resolveOutParam PMRelative = withNextValue $ \o -> do
  base <- gets relativeBase
  return $ base `offset` Offset (fromIntegral o)
resolveOutParam PMValue = error "Value mode for out parameter."

withNextValue :: Monad m => (Integer -> Interpreter m a) -> Interpreter m a
withNextValue f = do
  st <- get
  let v = readAt (position st) (memory st)
  res <- f v
  modify $ \s -> s { position = position st `offset` Offset 1 }
  return res

offset :: Address -> Offset -> Address
offset (Address a) (Offset o) = Address (a + o)

readAt :: Address -> Memory -> Integer
readAt a = fromMaybe 0 . Map.lookup a

setAt :: Address -> Integer -> Memory -> Memory
setAt = Map.insert

snapshotMemory :: Monad m => Interpreter m [Integer]
snapshotMemory = do
  st <- get
  let (Address pos) = position st
      poss = map Address [pos..pos+3]
      mem = memory st
      vs = map (flip readAt mem) poss
  return vs

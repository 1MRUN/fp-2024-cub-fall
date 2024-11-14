module HW.Eval where

import qualified Data.Map as M
import State.MyState
import HW.StackMachine

-- Possible errors during evaluation
data Error v
  = StackUnderflow (StackInstr v) -- Not enough numbers on the stack to process the instruction
  | VarUndefined String           -- The variable is not defined in the environment 
  | StackNotExhausted Stack       -- After the program has finished evaluation, the stack is not a singleton
  deriving (Show, Eq)

type Stack = [Int] -- The stack, holding integers

type Env v = M.Map v Int -- The environment, mapping variables to their values

-- The machine state consists of the stack and the variable environment.
data MachineState v = MachineState
  { getStack :: Stack,
    getEnv :: Env v
  }
  deriving (Show, Eq)

-- Run the compiled program on an empty stack and environment
initialState :: MachineState String
initialState = MachineState [] M.empty

-- Execute a single instruction. 
-- Successful evaluation does not produce any useful result: only the effect of modifying state matters. 
execInstr :: (Ord v, Show v) => StackInstr v -> MyState (MachineState v) (Either (Error v) ())
execInstr (PushNum x) = do
  modify $ \s -> s { getStack = x : getStack s }
  return $ Right ()

execInstr (PushVar k) = do
  env <- gets getEnv
  case M.lookup k env of
    Just v -> do
      modify $ \s -> s { getStack = v : getStack s }
      return $ Right ()
    Nothing -> return $ Left (VarUndefined (show k))

execInstr Add = do
  stack <- gets getStack
  case stack of
    (a:b:rest) -> do
      modify $ \s -> s { getStack = (a + b) : rest }
      return $ Right ()
    _ -> return $ Left (StackUnderflow Add)

execInstr (StoreVar k) = do
  stack <- gets getStack
  case stack of
    (v:rest) -> do
      modify $ \s -> s { getEnv = M.insert k v (getEnv s), getStack = rest }
      return $ Right ()
    _ -> return $ Left (StackUnderflow (StoreVar k)) 

execProgram :: (Ord v, Show v) => StackProgram v -> MachineState v -> Either (Error v) (MachineState v)
execProgram [] ms =
  let s = getStack ms in
  case s of 
    [_] -> Right ms
    _ -> Left $ StackNotExhausted s
execProgram (instr:rest) ms = 
  let (res, newState) = runMyState (execInstr instr) ms in
  case newState of
    Left err -> Left err
    Right () -> execProgram rest res

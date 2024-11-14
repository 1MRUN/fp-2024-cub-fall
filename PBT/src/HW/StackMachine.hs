module HW.StackMachine where

-- A single stack machine instruction 
data StackInstr v
  = PushNum Int -- Pushes a number onto the stack
  | PushVar v   -- Pushes the value of a variable onto the stack
  | Add         -- Pops two numbers, adds them, and pushes the result
  | StoreVar v  -- Stores the top of the stack as the value of a variable
  deriving (Show, Eq)

-- A program is just a sequence of instructions 
type StackProgram v = [StackInstr v]

-- Corresponds to the expression `let x = 5 + 10 in x + 2`
exampleProgram :: StackProgram String
exampleProgram = [PushNum 5, PushNum 10, Add, StoreVar "x", PushVar "x", PushNum 2, Add]

-- Addition expects at least two elements on the stack 
underflowExpr :: StackProgram v 
underflowExpr = [PushNum 13, Add]

-- At least one element on the stack should be there for pushing variables
undefVar :: StackProgram String 
undefVar = [PushVar "x"]

programs :: [StackProgram String]
programs = 
  [ exampleProgram 
  , underflowExpr
  , undefVar 
  ]
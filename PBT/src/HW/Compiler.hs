module HW.Compiler where

import Expr 
import HW.StackMachine

-- let x = 13 in
-- let y = 42 in
-- let x = (x + y) in
-- (x + y)
-- should compile into 
-- [PushNum 13,StoreVar "x",PushNum 42,StoreVar "y",PushVar "x",PushVar "y",Add,StoreVar "x",PushVar "x",PushVar "y",Add]

-- Compiler of an expression into machine instructions
compile :: Expr v -> StackProgram v
compile (Num v) = [PushNum v]
compile (Var v) = [PushVar v]
compile (Plus v1 v2) = compile v1 ++ compile v2 ++ [Add]
compile (Let a b c) = compile b ++ [StoreVar a] ++ compile c

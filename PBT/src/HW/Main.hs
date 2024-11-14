module HW.Main where

import Expr 
import HW.StackMachine
import HW.Compiler 
import HW.Eval 

runCompiler :: Expr String -> IO ()
runCompiler expr = do 
  print expr 
  runExecuter (compile expr)

runExecuter :: StackProgram String -> IO ()
runExecuter prog = do 
  print prog
  print $ execProgram prog initialState

main :: IO ()
main = do
  mapM_ runCompiler exprs
  mapM_ runExecuter programs 
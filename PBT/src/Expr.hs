{-# LANGUAGE FlexibleInstances #-}
module Expr where 

import Text.Printf (printf)

data Expr v
  = Num Int 
  | Var v 
  | Plus (Expr v) (Expr v)
  | Let v (Expr v) (Expr v)
-- let x = 13 in 
-- let y = 42 in 
-- x + y + 1

instance Show (Expr String) where 
  show (Num x) = show x 
  show (Var v) = v 
  show (Plus x y) = printf "(%s + %s)" (show x) (show y)
  show (Let v e b) = printf "let %s = %s in\n%s" v (show e) (show b)
 
instance Show (Expr Int) where 
  show (Num x) = show x 
  show (Var v) = printf "v.%d" v 
  show (Plus x y) = printf "(%s + %s)" (show x) (show y)
  show (Let v e b) = printf "let v.%d = %s in\n%s" v (show e) (show b)

expr = 
  Let "x" (Num 13) $ 
  Let "y" (Num 42) $ 
  Let "x" (Plus (Var "x") (Var "y")) (Plus (Var "x") (Var "y"))

expr1 = 
  Let "x" (Num 13) $ 
  Let "x" (Num 42) $ 
  Let "x" (Plus (Var "x") (Var "x")) (Plus (Var "x") (Var "x"))

exprs :: [Expr String]
exprs = [expr, expr1]
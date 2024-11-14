module State.ConservativeRenamer where 

import State.MyState ( MyState(runMyState), get, gets, put, modify ) 
import Expr ( Expr(..) ) 
import qualified Data.Map as M 
import Text.Printf 

data RenameState a = RenameState { getMap :: M.Map a String, getVars :: M.Map a Int }

-- let x = 13 in
-- let y = 42 in
-- let x = (x + y) in
-- (x + y)
-- Result: 
-- let x = 13 in
-- let y = 42 in
-- let x0 = (x + y) in
-- (x0 + y)

-- let x = 13 in
-- let x0 = 42 in
-- let x1 = (x0 + x0) in
-- (x1 + x1)
-- Result: 
-- let v.0 = 13 in
-- let v.1 = 42 in
-- let v.2 = (v.1 + v.1) in
-- (v.2 + v.2)



rename :: Expr String -> MyState (RenameState String) (Expr String)
rename (Num x) = return $ Num x 
rename (Plus x y) = do 
  x' <- rename x 
  y' <- rename y 
  return $ Plus x' y' 
  -- RenameState { getMap :: M.Map a String, getVars :: M.Map a Int }
rename (Var v) = do 
  varMap <- gets getMap 
  case M.lookup v varMap of 
    Just v' -> return $ Var v' 
    Nothing -> return $ Var v 
rename (Let v x b) = do 
  varCounters <- gets getVars 
  x' <- rename x 
  case M.lookup v varCounters of 
    Just i -> do 
      let v' = printf "%s%d" v i 
      modify (\s -> s { getMap = M.insert v v' (getMap s)
                      , getVars = M.insert v (i+1) (getVars s) } ) 
      b' <- rename b 
      return $ Let v' x' b' 
    Nothing -> do 
      modify ( \s -> s { getVars = M.insert v 0 (getVars s) } )
      b' <- rename b 
      return $ Let v x' b' 

runRename :: Expr String -> Expr String
runRename expr = 
  snd $ runMyState (rename expr) (RenameState { getMap = M.empty, getVars = M.empty })
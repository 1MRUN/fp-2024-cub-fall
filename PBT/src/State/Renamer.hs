module State.Renamer where 

import State.MyState ( MyState(runMyState), get, put, gets, modify ) 
import Expr ( Expr(..) )
import qualified Data.Map as M 

data RenameState a = RenameState { getMap :: M.Map a Int, getVar :: Int }

-- let x = 13 in
-- let y = 42 in
-- let x = (x + y) in
-- (x + y)

-- let v.0 = 13 in
-- let v.1 = 42 in
-- let v.2 = (v.0 + v.1) in
-- (v.2 + v.1)

rename :: (Ord a, Show a) => Expr a -> MyState (RenameState a) (Expr Int)
rename (Num x) = return $ Num x 
rename (Plus x y) = do 
  x' <- rename x 
  y' <- rename y 
  return $ Plus x' y' 
rename (Var v) = do 
  varMap <- gets getMap  
  case M.lookup v varMap of 
    Just v' -> return $ Var v' 
    Nothing -> error $ "Unbound variable " ++ show v 
  -- RenameState { getMap :: M.Map a Int, getVar :: Int }
rename (Let v x b) = do 
  varCount <- gets getVar 
  let nextVar = varCount + 1 
  x' <- rename x 
  modify (\s -> s {getVar = nextVar, getMap = M.insert v varCount (getMap s)})
  b' <- rename b 
  return $ Let varCount x' b' 

runRename :: (Ord a, Show a) => Expr a -> Expr Int
runRename expr = 
  snd $ runMyState (rename expr) (RenameState { getMap = M.empty, getVar = 0 })

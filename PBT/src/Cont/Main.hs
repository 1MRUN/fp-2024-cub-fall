module Cont.Main where 

import Cont.MyCont

fibCPS :: Int -> (Int -> r) -> r
fibCPS 0 k = k 0
fibCPS 1 k = k 1
fibCPS n k =
  fibCPS (n-1) $ \a ->
  fibCPS (n-2) $ \b ->
  k (a + b)

fib :: Int -> Int
fib n = fibCPS n id

fibMonad :: Int -> MyCont r Int 
fibMonad 0 = return 0 
fibMonad 1 = return 1 
fibMonad n = do 
  a <- fibMonad (n-1)
  b <- fibMonad (n-2)
  return (a + b)

runFib :: Int -> Int 
runFib = 
  evalMyCont . fibMonad 

main = 
  print $ runFib 10
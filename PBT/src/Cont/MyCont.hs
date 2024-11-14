{-# LANGUAGE InstanceSigs #-}
module Cont.MyCont where

newtype MyCont r a = MyCont { runMyCont :: (a -> r) -> r }

evalMyCont :: MyCont r r -> r  
evalMyCont m = undefined 

instance Functor (MyCont r) where
  fmap :: (a -> b) -> MyCont r a -> MyCont r b
  -- (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r)
  fmap f (MyCont x) = MyCont $ \k -> undefined 

instance Applicative (MyCont r) where 
  pure :: a -> MyCont r a 
  -- a -> ((a -> r) -> r)
  pure x = MyCont $ \k -> undefined 

  (<*>) :: MyCont r (a -> b) -> MyCont r a -> MyCont r b
  -- (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
  MyCont f <*> MyCont x = MyCont $ \k -> 
    undefined

instance Monad (MyCont r) where 
  (>>=) :: MyCont r a -> (a -> MyCont r b) -> MyCont r b 
  -- ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
  MyCont x >>= f = MyCont $ \k -> 
    undefined
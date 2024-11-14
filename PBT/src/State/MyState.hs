{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
module State.MyState where

newtype MyState s a = MyState { runMyState :: s -> (s, a) }

get :: MyState s s
get = MyState $ \s -> (s, s)

gets :: (s -> a) -> MyState s a
gets f = fmap f get -- get >>= \s -> return (f s) == fmap f get 

put :: s -> MyState s ()
put s = MyState $ const (s, ())

modify :: (s -> s) -> MyState s ()
modify f = do -- MyState $ \s -> (f s, ())
  s <- get
  put (f s) -- get >>= \s -> put (f s)

instance Functor (MyState s) where
  fmap :: (a -> b) -> MyState s a -> MyState s b
  fmap f (MyState g) = MyState $ \s -> -- s :: s 
    -- g :: s -> (s, a)
    let (s', a) = g s in
    (s', f a) -- :: (s, b)

-- newtype MyState s a = MyState {runMyState :: s -> (s, a)}

instance Applicative (MyState s) where
  pure :: a -> MyState s a
  pure x = MyState $ \s -> (s, x)

  (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
  MyState f <*> MyState x = MyState $ \s ->
    let (s', g) = f s in -- :: (s, a -> b) 
    let (s'', y) = x s' in -- :: (s, a)
    (s'', g y) -- :: (s, b)

instance Monad (MyState s) where
  (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  MyState f >>= m = MyState $ \s ->
    let (s', x) = f s in
    runMyState (m x) s' -- :: \s -> (s, b) -- :: MyState s b ~ \s -> (s, b)
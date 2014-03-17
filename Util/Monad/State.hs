module Util.Monad.State where

import Control.Monad

newtype State s a = State { runState :: (s -> (a, s)) }
 
instance Monad (State s) where
    return a        = State $ \s -> (a, s)
    (State x) >>= f  = State $ \s ->
        let (v, s') = x s
        in  runState (f v) s'

class Monad m => MonadState s m | m -> s where
    get :: m s
    get = state (\s -> (s, s))
    put :: s -> m ()
    put s = state (\_ -> ((), s))
    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get
        let ~(a, s') = f s
        put s'
        return a

newtype StateT s m a = StateT { runStateT :: (s -> m (a, s)) }

instance (Monad m) => Monad (StateT s m) where
    return a        = StateT $ \s -> return (a, s)
    (StateT x) >>= f = StateT $ \s -> do
        (v, s') <- x s          
        runStateT (f v) s'    

instance (Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero = StateT $ \s -> mzero
    (StateT x1) `mplus` (StateT x2) = StateT $ \s -> (x1 s) `mplus` (x2 s)


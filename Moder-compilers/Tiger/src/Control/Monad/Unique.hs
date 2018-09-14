{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Unique where


-- currently this code does not work as intended, as Ι don't
-- know how to get the Transformers to play nicely yet

-- import Trans
import Control.Monad.State.Strict
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except

class MonadUnique m where
  fresh :: m Int

newtype UniqueT m a = UniqueT (StateT Int m a)
  deriving(Functor, Applicative, Monad, MonadError e)

instance Monad m => MonadUnique (UniqueT m) where
  fresh = UniqueT (get <* modify (+ 1))


instance MonadUnique m => MonadUnique (StateT e m) where
  fresh = undefined

instance MonadUnique m => MonadUnique (ExceptT e m) where
  fresh = undefined

instance MonadState s m => MonadState s (UniqueT m) where
  state = undefined

-- instance MonadError e m => MonadError e (UniqueT m) where
--   throwError  = undefined
--   catchError  = undefined


runUniqueT (UniqueT t) = evalStateT t 0

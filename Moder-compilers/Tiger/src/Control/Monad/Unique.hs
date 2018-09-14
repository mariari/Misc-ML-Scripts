{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Unique where


-- Only works nicely in the one instance I'm using it
-- Was having issues with
-- StateT Translation (UniqueT (Except e)) a

-- however works fine
-- UniqueT (StateT Translation (Except e)) a

import Control.Monad.State.Strict
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except

class MonadUnique m where
  fresh :: m Int

newtype UniqueT m a = UniqueT (StateT Int m a)
  deriving(Functor, Applicative, Monad, MonadError e, MonadTrans, MonadIO)

instance Monad m => MonadUnique (UniqueT m) where
  fresh = UniqueT (get <* modify succ)

instance MonadState s m => MonadState s (UniqueT m) where
  state = lift . state

runUniqueT (UniqueT t) = evalStateT t 0

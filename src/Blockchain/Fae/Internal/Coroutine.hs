{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Coroutine 
  (
    module Blockchain.Fae.Internal.Coroutine,
    module C
  ) where

import Control.Monad.Coroutine as C
import Control.Monad.Coroutine.SuspensionFunctors as C

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{- Orphan instances -}

instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
  state = lift . state

instance (Functor f, MonadReader r m) => MonadReader r (Coroutine f m) where
  ask = lift ask
  local = liftT . local

instance (Functor f, MonadWriter w m) => MonadWriter w (Coroutine f m) where
  tell = lift . tell
  listen = liftT listen
  pass = liftT pass

{- Functions -}

liftT :: (Monad m, MonadTrans t, Monad (t m)) => (m a -> m b) -> (t m a -> t m b)
liftT f x = x >>= lift . f . return


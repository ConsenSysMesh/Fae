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

import Data.Bifunctor

{- Orphan instances -}

instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
  state = lift . state

instance (Functor f, MonadReader r m) => MonadReader r (Coroutine f m) where
  ask = lift ask
  local f (Coroutine xM) = Coroutine $ local f xM

instance (Functor f, MonadWriter w m) => MonadWriter w (Coroutine f m) where
  tell = lift . tell
  listen (Coroutine xM) = Coroutine $ do
    (e, w) <- listen xM
    return $ bimap (fmap $ fmap (,w)) (,w) e
  pass (Coroutine xfM) = Coroutine $ do
    e <- xfM
    case e of
      Left scr -> return $ Left (fmap pass scr)
      Right p -> fmap Right $ pass $ return p

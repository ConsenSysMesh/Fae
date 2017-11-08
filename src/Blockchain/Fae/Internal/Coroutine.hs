{- |
Module: Blockchain.Fae.Internal.Coroutine
Description: Wrapper library for "Control.Monad.Coroutine"
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module both nicely bundles "Control.Monad.Coroutine" and "Control.Monad.Coroutine.SuspensionFunctors", but it also defines some much-needed 'mtl' instances for 'Coroutine's.  Sadly, since @monad-coroutine@ dropped the ball here, these have to be orphan instances (and undecidable to boot), because I don't feel like wrapping 'Coroutine' in a @newtype@.
-}
{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Coroutine 
  (
    module Blockchain.Fae.Internal.Coroutine,
    module Control.Monad.Coroutine,
    module Control.Monad.Coroutine.SuspensionFunctors
  ) where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor

{- Orphan instances -}

-- | A 'Coroutine' on a 'MonadState' is a 'MonadState' just by lifting.
instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
  state = lift . state

-- | A 'Coroutine' on a 'MonadReader' can read just by lifting.  Making
-- 'local' changes is a little harder: you have to also make the change in
-- the continuation functor.
instance (Functor f, MonadReader r m) => MonadReader r (Coroutine f m) where
  ask = lift ask
  local f (Coroutine xM) = Coroutine $ local f $ fmap (local1 f) xM
    where local1 f = bimap (fmap $ local f) id

-- | A 'Coroutine' on a 'MonadWriter' can write just by lifting.
-- 'listen'ing must also listen to the continuation functor.  'pass'ing
-- passes both the value and the continuation.
instance (Functor f, MonadWriter w m) => MonadWriter w (Coroutine f m) where
  tell = lift . tell
  listen (Coroutine xM) = Coroutine $ do
    (cr, w) <- listen $ fmap listen1 xM
    return $ fmap (,w) cr
    where listen1 = bimap (fmap listen) id
  pass (Coroutine xfM) = Coroutine $ do
    e <- xfM
    case e of
      Left scr -> return $ Left (fmap pass scr)
      Right p -> fmap Right $ pass $ return p

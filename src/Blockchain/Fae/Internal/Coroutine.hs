{- |
Module: Blockchain.Fae.Internal.Coroutine
Description: Wrapper library for "Control.Monad.Coroutine"
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module both nicely bundles "Control.Monad.Coroutine" and "Control.Monad.Coroutine.SuspensionFunctors", but it also defines some much-needed 'mtl' instances for 'Coroutine's.  Sadly, since @monad-coroutine@ dropped the ball here, these have to be orphan instances (and undecidable to boot), because wrapping 'Coroutine' in a newtype turns out to be surprisingly messy due to its recursive definition.
-}
{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Coroutine 
  (
    module Blockchain.Fae.Internal.Coroutine,
    module Control.Monad.Coroutine,
    module Control.Monad.Coroutine.SuspensionFunctors
  ) where

import Control.Monad.Catch

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor

-- | Lazy specialization of 'mapMonad' for @'Coroutine' 'Naught'@,
-- isomorphic to the identity functor and therefore not actually a sum
-- type.
mapMonadNaught :: 
  (Monad m, Monad n) =>
  (forall x. m x -> n x) -> Coroutine Naught m a -> Coroutine Naught n a
mapMonadNaught f (Coroutine eithM) = Coroutine $ fmap f' $ f eithM where
  f' ~(Right x) = Right x

{- Orphan instances -}

-- | Laziness optimization for @'Coroutine' 'Naught'@, isomorphic to 'Identity'.
instance {-# OVERLAPPING #-} 
  (Functor m) => Functor (Coroutine Naught m)
  where

  fmap f (Coroutine eithM) = Coroutine $ fmap f' eithM where 
    f' ~(Right x) = Right $ f x

-- | Laziness optimization for @'Coroutine' 'Naught'@, isomorphic to 'Identity'.
instance {-# OVERLAPPING #-}
  (Applicative m) => Applicative (Coroutine Naught m)
  where

  pure = Coroutine . pure . Right
  (Coroutine fM) <*> (Coroutine eithM) = Coroutine $ fmap f' fM <*> eithM where 
    f' ~(Right f) ~(Right x) = Right $ f x

-- | Laziness optimization for @'Coroutine' 'Naught'@, isomorphic to 'Identity'.
instance {-# OVERLAPPING #-}
  (Monad m) => Monad (Coroutine Naught m)
  where

  (Coroutine eithM) >>= f = Coroutine $ eithM >>= f' where
    f' ~(Right x) = resume $ f x

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

-- | A 'Coroutine' can 'throw' just as a plain value without continuation.
instance (Functor f, MonadThrow m) => MonadThrow (Coroutine f m) where
  throwM = Coroutine . fmap Right . throwM 

-- | A 'Coroutine' can 'catch' by catching the regular value and passing
-- along the attempt to the continuation.
instance (Functor f, MonadCatch m) => MonadCatch (Coroutine f m) where
  catch (Coroutine mE) ef = Coroutine $ do
    xE <- catch mE $ resume . ef
    return $ bimap (fmap $ flip catch ef) id xE


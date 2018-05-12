{- |
Module: Blockchain.Fae.Internal.Suspend
Description: Suspend and resume for contracts
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module implements functions that can be suspended and resumed, using
the continuation monad.
-}
module Blockchain.Fae.Internal.Suspend where

import Control.DeepSeq

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Writer

import Data.Monoid

-- * Types

-- | A monad transformer suitable for creating a suspendable function.
newtype SuspendT a b m c = SuspendT { getSuspendT :: ContT b (NextT a b m) c }
  deriving (Functor, Applicative, Monad)
-- | The precursor to a 'SuspendStepF', the form in which a suspendable
-- function will actually be written.
type SuspendPreF a b m = a -> SuspendT a b m b

-- | The monad that stores the next suspension
type NextT a b m = WriterT (Last (SuspendF a b m)) m
-- | An abstract suspendable function
newtype SuspendF a b m = SuspendF { getSuspendF :: a -> NextT a b m b }

-- | A concrete suspendable function
newtype SuspendStepF a b m = 
  SuspendStepF { getSuspendStepF :: a -> m (b, Maybe (SuspendStepF a b m)) }
  deriving (NFData)

{- Instances -}

-- | -
instance MonadTrans (SuspendT a b) where
  lift = SuspendT . lift . lift

-- * Actions

-- | This allows you to mark the boundaries of the successive runs of the
-- function.  The current call will end when it reaches a 'suspend',
-- returning the argument to 'suspend', and evaluating to the argument of
-- the next call.
suspend :: (Monad m) => b -> SuspendT a b m a
suspend y = SuspendT . ContT $ \cont -> do
  tell . Last . Just . SuspendF $ cont
  return y

-- | This allows you to mark the end of the computation, returning a value
-- immediately and ignoring subsequent code.
terminate :: (Monad m) => b -> SuspendT a b m b
terminate y = SuspendT . ContT . const . pass $ return (y, const $ Last Nothing)

-- * Meta-operations

-- | Transforms a monadically-defined suspendable function into a concrete
-- one.
startSuspendF :: (Monad m) => SuspendPreF a b m -> SuspendStepF a b m
startSuspendF f = stepSuspendF . SuspendF $ evalContT . getSuspendT . f

-- | Utility function: makes the abstract concrete.
stepSuspendF :: (Functor m) => SuspendF a b m -> SuspendStepF a b m
stepSuspendF f = SuspendStepF $ 
  fmap (fmap (fmap stepSuspendF . getLast)) . runWriterT . getSuspendF f

-- | Alters the parameters of the suspended function.
alterSuspendStepF ::
  (Monad m, Monad m') => 
  (a' -> m' a) -> (b -> m' b') -> (forall c. m c -> m' c) ->
  SuspendStepF a b m -> SuspendStepF a' b' m'
alterSuspendStepF fArg fVal fMon g = 
  SuspendStepF $ fArg >=> fMon . getSuspendStepF g >=> fVal' 
  where fVal' (y, sfM) = (, alterSuspendStepF fArg fVal fMon <$> sfM) <$> fVal y


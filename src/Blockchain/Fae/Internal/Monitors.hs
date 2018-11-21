module Blockchain.Fae.Internal.Monitors where

import Blockchain.Fae.Internal.Exceptions

import Control.Concurrent
import Control.DeepSeq

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Maybe

import System.Posix.Process
import System.Posix.Signals

import System.Timeout
   
-- | An 'evaluate'-like function, e.g. a timeout or memory usage limit.
data EvalF m = EvalF { getEvalF :: forall a. (NFData a) => a -> m a }
-- | A global static 'EvalT'.
type EvalT m = ReaderT (EvalF m) m

-- | Apply a global function directly to an argument.
evalArg :: (Monad m, NFData a) => a -> EvalT m a
evalArg x = asks getEvalF >>= lift . ($ x)

-- | Initialize 'EvalT' with a timeout 'EvalF'.
evalTimed :: (MonadIO m) => Int -> EvalT m a -> m a
evalTimed evalTimeout = flip runReaderT (EvalF $ timed evalTimeout) where

-- | Basic impure timeout function.
timed :: (MonadIO m, NFData a) => Int -> a -> m a
timed evalTimeout = 
  liftIO . fmap (fromMaybe err) . cloak . 
  posixTimeout evalTimeout . evaluate . force 
  where 
    err = throw $ Timeout evalTimeout
    cloak = handleAll (return . throw)

-- | GHC has an actual /bug/ in its concurrency that non-allocating
-- evaluations can never be pre-empted, so the stock @timeout@ function
-- from @System.Timeout@ does not work on completely general values.  This
-- implementation is much, much slower because it forks a process rather
-- than sparking a thread, but it does not have that particular
-- limitation.
--
-- The timeout is in half-milliseconds because the fork takes that order of
-- magnitude of time, and in fact, it is incurred /twice/ because (in order
-- to avoid having to communicate the value between the processes) the
-- evaluation happens twice: once to check that it terminates, and once to
-- get the actual value.
posixTimeout :: Int -> IO a -> IO (Maybe a)
posixTimeout mSec act = do
  pID <- forkProcess $ void act
  stMM <- timeout (mSec * halfMillisecond) $ getProcessStatus True True pID
  case stMM of
    Nothing -> do
      signalProcess killProcess pID
      return Nothing
    _ -> Just <$> act
  where halfMillisecond = 500

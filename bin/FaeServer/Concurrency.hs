{- |
Module: FaeServer.Concurrency
Description: Concurrency-related constructs for faeServer
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The concurrency aspects of @faeServer@ are all centered around the Fae
interpreter, which runs in a dedicated thread that reads transactions (and
other instructions) to execute from a queue defined here.
-}

module FaeServer.Concurrency where

import Blockchain.Fae.FrontEnd

import Common.ProtocolT

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

import Data.Map (Map)

import System.Exit

-- | All the pieces of data that are required to execute a transaction in
-- the dedicated interpreter thread.
data TXExecData =
  TXExecData
  {
    mainFile :: Module,
    modules :: ModuleMap,
    parentM :: Maybe TransactionID,
    lazy :: Bool,
    fake :: Bool,
    tx :: TX,
    resultVar :: TMVar String,
    callerTID :: ThreadId
  } |
  View 
  {
    viewTXID :: TransactionID,
    parentM :: Maybe TransactionID,
    resultVar :: TMVar String,
    callerTID :: ThreadId
  } |
  ExportValue
  {
    parentM :: Maybe TransactionID,
    calledInTX :: TransactionID,
    ixInTX :: Int,
    exportResultVar :: TMVar ExportData,
    callerTID :: ThreadId
  } |
  ImportValue
  {
    parentM :: Maybe TransactionID,
    exportData :: ExportData,
    signalVar :: TMVar (),
    callerTID :: ThreadId
  }

-- | Communications channel with the interpreter thread
type TXQueue = TQueue TXExecData

-- | Context in which transactions can be placed for execution
type TXQueueT = ReaderT TXQueue

-- | An mtl-style class for things with a 'TXQueue'.  Its instances are all
-- trivially 'lift'ing in various monad transformers.
class (MonadIO m) => TXQueueM m where
  liftTXQueueT :: TXQueueT IO a -> m a

-- | -
instance {-# OVERLAPPING #-} TXQueueM (TXQueueT IO) where
  liftTXQueueT = id

-- | -
instance (TXQueueM m) => TXQueueM (ReaderT r m) where
  liftTXQueueT = lift . liftTXQueueT

-- | -
instance (TXQueueM m, Monoid w) => TXQueueM (WriterT w m) where
  liftTXQueueT = lift . liftTXQueueT

-- | -
instance (TXQueueM m) => TXQueueM (StateT s m) where
  liftTXQueueT = lift . liftTXQueueT

-- | -
instance (TXQueueM m) => TXQueueM (ContT r m) where
  liftTXQueueT = lift . liftTXQueueT

instance (TXQueueM m) => TXQueueM (FaeInterpretT m) where
  liftTXQueueT = lift . liftTXQueueT

instance (TXQueueM m) => TXQueueM (ProtocolT m) where
  liftTXQueueT = ProtocolT . liftTXQueueT

-- | Abstraction for "doing something with" a transaction
type SendTXExecData m = TXExecData -> m ()

-- | Blocks on the result variable after sending off the TX.
waitRunTXExecData :: 
  (TXQueueM m) => 
  SendTXExecData m -> (TXExecData -> TMVar a) -> TXExecData -> m a
waitRunTXExecData sendOff tmVar txExecData = do
  sendOff txExecData
  ioAtomically $ takeTMVar $ tmVar txExecData

-- | Sends the TX by simply placing it in the queue.
queueTXExecData :: (TXQueueM m) => SendTXExecData m
queueTXExecData txExecData = do
  txQueue <- liftTXQueueT ask
  ioAtomically $ writeTQueue txQueue txExecData

-- | Checks the queue for a new item, blocking until it finds one.
readTXExecData :: (TXQueueM m) => m TXExecData
readTXExecData = do
  txQueue <- liftTXQueueT ask   
  ioAtomically $ readTQueue txQueue

-- | Handles exceptions in the mixed situation where /most/ of them have to
-- go back to the calling thread (i.e. the one with the connection that
-- created the present 'TXExecData' item), except the ones that would
-- directly exit the process, which have to go to the main thread so that,
-- e.g. Ctrl-C works on the terminal.
reThrowExit :: (MonadIO m, MonadCatch m) => ThreadId -> ThreadId -> m () -> m ()
reThrowExit mainTID callerTID =
  reThrow callerTID . handle (liftIO . throwTo @ExitCode mainTID)

-- | Just bounces exceptions to a different thread.  The interpreter thread
-- must never terminate, and the victim is whichever Warp thread
-- corresponds to the presently executing item.
reThrow :: (MonadIO m, MonadCatch m) => ThreadId -> m () -> m ()
reThrow tID = handleAll (liftIO . throwTo tID)

-- | Though barely shorter than its definition, this does save having to
-- reproduce the (trivial) particular logic each time.
ioAtomically :: (MonadIO m) => STM a -> m a
ioAtomically = liftIO . atomically


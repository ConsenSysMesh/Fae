{- |
Module: FaeServer.Fae
Description: The Fae interpreter in faeServer
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module is essentially the interpreter thread.  It reads from the
'TXQueue' to which all the Warp threads write, updating an in-memory
history with each new state or import, or reading from that history for
viewing or export.
-}

module FaeServer.Fae where

import Blockchain.Fae.FrontEnd

import Common.JSON

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception.Base

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Cont

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import FaeServer.Args
import FaeServer.Concurrency
import FaeServer.Git
import FaeServer.History
import FaeServer.Modules

import System.Directory
import System.FilePath
import System.IO

-- | Not just an infinite event loop, this also handles initializing the
-- storage from cached transactions when a new session was not requested,
-- or clearing that cache when it was.  It is guaranteed never to die from
-- an exception (unless that exception is actually uncatchable), though any
-- exception that is only caught here is bounced to the main thread and,
-- thus, ends the program.  Once in the event loop, only actual 'ExitCode'
-- exceptions can terminate the program.
runFae :: ThreadId -> ServerArgs -> TXQueueT IO ()
runFae mainTID ServerArgs{..} = reThrow mainTID $ runFaeInterpretWithHistory $ do
  if newSession 
  then liftIO $ do
    removePathForcibly "Blockchain"
    removePathForcibly "txcache"
    createDirectory "txcache"
    gitInit 
  -- Note that this case runs transactions but does not catch exceptions as
  -- the main loop does.  This is because it runs them lazily, and so the
  -- exceptions are never thrown.
  else forM_TXCache $ \tx@TX{..} parentM -> do
    txCount <- innerRun tx parentM gitReset
    incrementHistory txID txCount
    liftIO $ putStrLn $ 
      "Replayed transaction " ++ show txID ++ " (#" ++ show txCount ++ ")"
  forever $ do
    txExecData <- readTXExecData
    reThrowExit mainTID (callerTID txExecData) $ 
      runTXExecData evalTimeout txExecData

-- | Gives operational meaning to the various 'TXExecData' alternatives.
runTXExecData :: 
  (Typeable m, MonadIO m, MonadMask m) => 
  Int -> TXExecData -> FaeInterpretWithHistoryT m ()
-- This, the normal case, surrounds actually running the transaction with
-- a great deal of bookkeeping to build the longest chain and save and
-- restore history from the correct point in time.
runTXExecData evalTimeout TXExecData{tx=tx@TX{..}, ..} = do
  dup <- gets $ Map.member txID . txStorageAndCounts
  when dup $ throw $ ErrorCall $ "Duplicate transaction ID: " ++ show txID

  txCount <- innerRun tx parentM (writeModules mainFile modules)
  txResult <-
    if lazy
    then return $ "Transaction " ++ show txID ++ " (#" ++ show txCount ++ ")"
    else do
      txSummary <- lift . evalTimed evalTimeout $ collectTransaction txID
      -- Strict because I've seen a case where an exception is thrown so
      -- late that it isn't caught by the app and a response isn't even sent.
      return $! encodeJSON txSummary
  if fake
  then unless lazy $ liftIO gitClean 
  else do
    incrementHistory txID txCount
    extendTXCache tx parentM
    liftIO $ gitCommit txID
  ioAtomically $ putTMVar resultVar txResult

-- For a 'View', we just roll back the history and build the transaction
-- summary.
runTXExecData evalTimeout View{..} = do
  void $ recallHistory parentM
  txSummary <- lift . evalTimed evalTimeout $ collectTransaction viewTXID
  ioAtomically $ putTMVar resultVar (encodeJSON txSummary)

-- 'ExportValue' is quite similar to 'View', except an 'ExportData' is
-- built instead.
runTXExecData _ ExportValue{..} = do
  void $ recallHistory parentM
  exportResult <- lift $ lift $ getExportedValue calledInTX ixInTX
  ioAtomically $ putTMVar exportResultVar exportResult

-- 'ImportValue' is just the bookkeeping part of the 'TXExecData' case's
-- operation, calling out to the interpreter to insert the imported value
-- directly rather than running a new transaction.
runTXExecData _ ImportValue{..} = do
  parentCount <- recallHistory parentM
  lift $ interpretImportedValue exportData
  updateHistory parentM parentCount
  ioAtomically $ putTMVar signalVar ()

-- | The interpreter is actually run here; this appears both in the initial
-- fast-forward from the transaction cache and in 'runTXExecData'.
innerRun :: 
  (Typeable m, MonadIO m, MonadMask m) =>
  TX -> Maybe TransactionID -> (TransactionID -> IO ()) ->
  FaeInterpretWithHistoryT m Integer
innerRun tx@TX{..} parentM placeModules = do
  txCount <- recallHistory parentM
  liftIO $ placeModules txID
  lift $ interpretTX tx
  return txCount

-- | Appends a new transaction to the cache.  The cache is not very
-- complicated: it has a file for each transaction object, and a line in
-- a single file for each transaction ID, in order of appearance,
-- regardless of how they depend on earlier ones.
extendTXCache :: (MonadIO m) => TX -> Maybe TransactionID -> m ()
extendTXCache tx@TX{..} parentM = liftIO $ do
  B.writeFile (makeTXFileName txID) $ S.encode (tx, parentM)
  B.appendFile indexFileName $ S.encode txID

-- | Does...something...to each transaction in the cache.  Though the
-- definition is somewhat extended, most of it is just handling filesystem
-- stuff.
forM_TXCache :: (MonadIO m) => (TX -> Maybe TransactionID -> m ()) -> m ()
forM_TXCache f = evalContT $ callCC $ \done -> do
  let 
    act h = go where
      hGetS :: (Serialize a, MonadIO m) => Int -> m a
      hGetS = decodeFile (error "Bad TX cache") (liftIO . B.hGet h)
      go = do
        atEOF <- liftIO $ hIsEOF h
        when atEOF $ done ()
        txID <- hGetS txIDLength
        let txErr = error $ "Bad transaction file for TX" ++ show txID
        (tx, parentM) <- getTX txErr $ makeTXFileName txID
        lift $ f tx parentM
        go
  isCache <- liftIO $ doesFileExist indexFileName
  unless isCache $ done ()
  h <- liftIO $ openBinaryFile indexFileName ReadMode
  act h
  liftIO $ hClose h

  where 
    decodeFile :: (Serialize a, Monad m) => a -> (b -> m ByteString) -> (b -> m a)
    decodeFile err getter = fmap (either (const err) id . S.decode) . getter
    getTX err = decodeFile err (liftIO . B.readFile)

-- | Establishes the location of the transaction cache's index file.
indexFileName :: String
indexFileName = "txcache" </> "index"

-- | Establishes the location of the transaction cache files.
makeTXFileName :: TransactionID -> String
makeTXFileName txID = "txcache" </> show txID

-- | A constant that, no doubt, is equal to 32 (bytes).
txIDLength :: Int
txIDLength = B.length $ S.encode nullID


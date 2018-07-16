module FaeServer.Fae where

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception.Base

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import qualified Data.Map as Map

import FaeServer.Args
import FaeServer.Concurrency
import FaeServer.Git
import FaeServer.History
import FaeServer.Modules

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

runFae :: Int -> ThreadId -> Flags -> TXQueueT IO ()
runFae port mainTID Flags{..} = reThrow mainTID $ runFaeInterpretWithHistory $ do
  liftIO $ do
    createDirectoryIfMissing False portDir
    setCurrentDirectory portDir

  if newSession 
  then liftIO $ do
    removePathForcibly "Blockchain"
    removePathForcibly "txcache"
    createDirectory "txcache"
    gitInit 
  else forM_TXCache $ \tx@TX{..} parentM -> do
    txCount <- innerRun tx parentM gitReset
    updateHistory (Just txID) txCount
    liftIO $ putStrLn $ 
      "Replayed transaction " ++ show txID ++ " (#" ++ show txCount ++ ")"
  forever $ do
    txExecData <- readTXExecData
    reThrowExit mainTID (callerTID txExecData) $ runTXExecData txExecData

  where portDir = "port-" ++ show port

runTXExecData :: 
  (Typeable m, MonadIO m, MonadMask m) => 
  TXExecData -> FaeInterpretWithHistoryT m ()
runTXExecData TXExecData{tx=tx@TX{..}, ..} = do
  dup <- gets $ Map.member txID . txStorageAndCounts
  when dup $ throw $ ErrorCall $ "Duplicate transaction ID: " ++ show txID

  txCount <- innerRun tx parentM (writeModules mainFile modules)
  txResult <-
    if lazy
    then return $ "Transaction " ++ show txID ++ " (#" ++ show txCount ++ ")"
    else lift $ showTransaction txID
  if fake
  then unless lazy $ liftIO gitClean 
  else do
    updateHistory (Just txID) (txCount + 1)
    extendTXCache tx parentM
    liftIO $ gitCommit txID
  ioAtomically $ putTMVar resultVar txResult

runTXExecData View{..} = do
  void $ recallHistory parentM
  txResult <- lift $ showTransaction viewTXID
  ioAtomically $ putTMVar resultVar txResult

runTXExecData ExportValue{..} = do
  void $ recallHistory parentM
  exportResult <- lift $ lift $ getExportedValue calledInTX shortCID
  ioAtomically $ putTMVar exportResultVar exportResult

runTXExecData ImportValue{..} = do
  parentCount <- recallHistory parentM
  lift $ interpretImportedValue importedCID valueType valuePackage
  updateHistory parentM parentCount
  ioAtomically $ putTMVar signalVar ()

innerRun :: 
  (Typeable m, MonadIO m, MonadMask m) =>
  TX -> Maybe TransactionID -> (TransactionID -> IO ()) ->
  FaeInterpretWithHistoryT m Integer
innerRun tx@TX{..} parentM placeModules = do
  txCount <- recallHistory parentM
  liftIO $ placeModules txID
  lift $ interpretTX tx
  return txCount

extendTXCache :: (MonadIO m) => TX -> Maybe TransactionID -> m ()
extendTXCache tx@TX{..} parentM = liftIO $ do
  B.writeFile (makeTXFileName txID) $ S.encode (tx, parentM)
  B.appendFile indexFileName $ S.encode txID

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

indexFileName :: String
indexFileName = "txcache" </> "index"

makeTXFileName :: TransactionID -> String
makeTXFileName txID = "txcache" </> show txID

txIDLength :: Int
txIDLength = B.length $ S.encode nullID


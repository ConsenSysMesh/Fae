module FaeServer.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Concurrent.Lifted
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Cont

import Data.Aeson 
  (
    FromJSON(..), ToJSON(..), 
    genericToEncoding, Options(..), defaultOptions, 
    (.:), (.:?)
  )
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Aeson.Parser
import Data.Monoid

import FaeServer.App
import FaeServer.Fae
import FaeServer.Concurrency
import FaeServer.Modules

import GHC.Generics

import System.IO

data EthNewBlock =
  EthNewBlock
  {
    ethSubID :: Hex,
    ethBlockE :: Either Error PartialEthBlock
  }

data PartialEthBlock =
  PartialEthBlock
  {
    ethBlockNumber :: HexInteger,
    ethBlockHash :: EthBlockID,
    ethParentHash :: EthBlockID,
    ethBlockTXs :: [PartialEthTransaction]
  }

data PartialEthTransaction =
  PartialEthTransaction
  {
    ethTXTo :: EthAddress,
    ethValue :: HexInteger,
    ethTXData :: FaeTX,
    ethTXID :: EthTXID
  }

data ParitySubscribe = ParitySubscribe
data EthGetBlockByHash = EthGetBlockByHash EthBlockID

type BlockLastTXs = Map EthBlockID TransactionID
newtype FaethWatcherM a = 
  FaethWatcherM
  {
    getFaethWatcherM :: 
      ReaderT (ThreadId, Hex, TVar BlockLastTXs) (ProtocolT (TXQueueT IO)) a
  }
  deriving
  (
    Functor, Applicative, Monad, 
    MonadThrow, MonadCatch, MonadIO,
    MonadProtocol, TXQueueM,
    MonadReader (ThreadId, Hex, TVar BlockLastTXs)
  )

instance MonadState BlockLastTXs FaethWatcherM where
  state f = do
    sVar <- view _3
    s <- liftIO $ readTVarIO sVar
    let (result, s') = f s
    ioAtomically $ writeTVar sVar s'
    return result
  get = do
    sVar <- view _3
    liftIO $ readTVarIO sVar
  put s = do
    sVar <- view _3
    ioAtomically $ writeTVar sVar s

instance ToJSON ParitySubscribe where
  toJSON ParitySubscribe = 
    toJSON 
    [
      "eth_getBlockByNumber", 
      toJSON ["latest", toJSON True]
    ]

instance ToJSON EthGetBlockByHash where
  toJSON (EthGetBlockByHash hash) = toJSON [toJSON hash, toJSON True]

instance FromJSON EthNewBlock where
  parseJSON = A.withObject "EthNewHead" $ \obj -> do
    ethSubID <- obj .: "subscription"
    errorM <- obj .:? "error"
    resultM <- obj .:? "result"
    let
      errorE = Left $ fromMaybe (error "Invalid response") errorM
      ethBlockE = maybe errorE Right resultM
    return EthNewBlock{..}

instance FromJSON PartialEthBlock where
  parseJSON = A.withObject "PartialEthBlock" $ \obj ->
    PartialEthBlock 
    <$> obj .: "number"
    <*> obj .: "hash"
    <*> obj .: "parentHash"
    <*> obj .: "transactions"
    
instance FromJSON PartialEthTransaction where
  parseJSON = A.withObject "PartialEthTransaction" $ \obj ->
    PartialEthTransaction
    <$> obj .: "to"
    <*> obj .: "value"
    <*> obj .: "input"
    <*> obj .: "hash"

instance ToRequest EthNewBlock where
  requestMethod = const "parity_subscription"

instance ToRequest ParitySubscribe where
  requestMethod = const "parity_subscribe"

instance ToRequest EthGetBlockByHash where
  requestMethod = const "eth_getBlockByHash"

runFaeth :: ThreadId -> TXQueueT IO ()
runFaeth mainTID = reThrow mainTID $ do
  fork $ runFaethWatcherM faethWatcher 
  runFaeServer faethSendTXExecData 

faethSendTXExecData :: SendTXExecData (TXQueueT IO) 
faethSendTXExecData txED@TXExecData{} = queueTXExecData txED{fake = True}
faethSendTXExecData txED = queueTXExecData txED

faethWatcher :: FaethWatcherM ()
faethWatcher = forever $ receiveSubscription >>= processNewBlock

processNewBlock :: PartialEthBlock -> FaethWatcherM TransactionID
processNewBlock b@PartialEthBlock{ethBlockHash} = do
  thisBlockHashM <- use $ at ethBlockHash
  maybe (addNewBlock b) return thisBlockHashM

addNewBlock :: PartialEthBlock -> FaethWatcherM TransactionID
addNewBlock b@PartialEthBlock{..} = do
  lastTXIDM <- use $ at ethParentHash
  lastTXID <- maybe (recurseBlock b) return lastTXIDM
  thisTXID <- processEthTXs ethBlockTXs lastTXID
  modify $ Map.insert ethBlockHash thisTXID
  return thisTXID

recurseBlock :: PartialEthBlock -> FaethWatcherM TransactionID
recurseBlock PartialEthBlock{..}
  | ethBlockNumber == 0 = return nullID
  | otherwise = do
      parentBlock <- sendReceiveProtocolT (EthGetBlockByHash ethParentHash)
      addNewBlock parentBlock

processEthTXs :: 
  [PartialEthTransaction] -> TransactionID -> FaethWatcherM TransactionID
processEthTXs ethBlockTXs lastTXID = do
  faethEthAddress <- askAddress
  foldl (>>=) (return lastTXID) $ 
    map processEthTX $
      filter ((faethEthAddress ==) . ethTXTo) ethBlockTXs

processEthTX :: 
  PartialEthTransaction -> TransactionID -> FaethWatcherM TransactionID
processEthTX PartialEthTransaction{..} lastTXID = handleAll ethTXError $ do
  case (ethValue >=) <$> feeM of
    Just False -> error $
      "Insufficient Ether provided: " ++ 
      "needed " ++ show (fromJust feeM) ++ "; got " ++ show ethValue
    _ -> return ()
  runFaethTX ethTXID ethTXData lastTXID

  where
    feeM = ethFee . faethSalt . faeTXMessage $ ethTXData
    ethTXError e = do
      liftIO . putStrLn $
        "\nError while processing Ethereum transaction " ++ show ethTXID ++
        "\nError was: " ++ show e ++ "\n"
      return lastTXID

runFaethTX :: Hex -> FaeTX -> TransactionID -> FaethWatcherM TransactionID
runFaethTX ethTXID (FaeTX txMessage mainFile0 modules0) lastTXID = do
  resultVar <- ioAtomically newEmptyTMVar
  callerTID <- view _1
  handleAll (execError thisTXID) $ do
    txResult <- waitRunTXExecData queueTXExecData
      TXExecData
      {
        parentM = Just lastTXID,
        lazy = True,
        fake = False,
        reward = False, -- When can this be True?
        ..
      }
    liftIO $ putStrLn txResult
  return thisTXID

  where
    tx = maybe (error "Invalid transaction message") id $ 
      txMessageToTX txMessage
    thisTXID = txID tx
    mainFile = addHeader thisTXID mainFile0
    modules = Map.mapWithKey (fixHeader thisTXID) modules0
    execError txID e = liftIO . putStrLn $
      "\nError while executing Fae transaction " ++ show txID ++
      "\n              in Ethereum transaction " ++ show ethTXID ++
      "\nError was: " ++ show e ++ "\n"

runFaethWatcherM :: FaethWatcherM () -> TXQueueT IO ()
runFaethWatcherM xFW = do
  EthAccount{..} <- readAccount "faeth"
  tID <- myThreadId
  blockTXIDs <- liftIO $ newTVarIO Map.empty
  forever $ handleAll waitRestart $ 
    runProtocolT address $ do
      subID <- sendReceiveProtocolT ParitySubscribe
      runReaderT (getFaethWatcherM xFW) (tID, subID, blockTXIDs) 

  where
    waitRestart e = liftIO $ do
      putStrLn $
        "Faeth watcher threw an error: " ++ show e ++
        "\nWaiting 30s to restart.  Press Enter to continue immediately."
      isInput <- hWaitForInput stdin (30 * 10^3)
      when isInput discard 
    discard = do
      more <- hReady stdin
      when more $ do
        void $ hGetChar stdin
        discard

receiveSubscription :: FaethWatcherM PartialEthBlock
receiveSubscription = do
  EthNewBlock{..} <- receiveRequest "parity_subscription"
  subID <- view _2
  unless (ethSubID == subID) $ error $
    "parity_subscribe notification has subscription ID " ++ show ethSubID ++
    "; expected " ++ show subID
  either (error . errMessage) return ethBlockE


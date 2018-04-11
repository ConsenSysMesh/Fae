module FaeServer.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Concurrent.Lifted
import Control.Concurrent.STM.TMVar

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Cont

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?))
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text.Read as T

import Data.Aeson.Parser
import Data.Monoid

import FaeServer.App
import FaeServer.Fae
import FaeServer.Concurrency
import FaeServer.Modules

import GHC.Generics

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

newtype HexInteger = HexInteger { getHexInteger :: Integer }
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral) 

type FaethM = ProtocolT (TXQueueT IO)
type FaethWatcherM = 
  ContT () (StateT BlockLastTXs (ReaderT (ThreadId, Hex) FaethM))
type BlockLastTXs = Map EthBlockID TransactionID

instance FromJSON HexInteger where
  parseJSON x = either error fst . T.hexadecimal <$> parseJSON x

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
  EthAccount{..} <- readAccount "faeth"
  tID <- myThreadId
  fork $ runProtocolT address $ runFaethWatcher mainTID tID
  runFaeServer faethSendTXExecData 

faethSendTXExecData :: SendTXExecData (TXQueueT IO) 
faethSendTXExecData txED@TXExecData{} = queueTXExecData txED{fake = True}
faethSendTXExecData txED = queueTXExecData txED

runFaethWatcher :: ThreadId -> ThreadId -> FaethM ()
runFaethWatcher mainTID tID = 
  reThrow mainTID $ runFaethWatcherM tID $ 
    forever $ receiveSubscription >>= void . recurseBlocks

recurseBlocks :: PartialEthBlock -> FaethWatcherM TransactionID
recurseBlocks PartialEthBlock{..} = do
  lastTXIDM <- use $ at ethParentHash
  lastTXID <-
    case lastTXIDM of
      x | Just txID <- x -> return txID
      Nothing 
        | ethBlockNumber == 0 -> return nullID
        | otherwise -> do
            parentBlock <- either (error . show) id <$> 
              sendReceiveProtocolT (EthGetBlockByHash ethParentHash)
            recurseBlocks parentBlock
  thisTXID <- processEthTXs ethBlockTXs lastTXID
  at ethBlockHash ?= thisTXID
  return thisTXID

processEthTXs :: 
  [PartialEthTransaction] -> TransactionID -> FaethWatcherM TransactionID
processEthTXs ethBlockTXs lastTXID = do
  faethEthAddress <- askAddress
  foldl (>>=) (return lastTXID) $ 
    map processEthTX $
      filter ((faethEthAddress ==) . ethTXTo) ethBlockTXs

processEthTX :: 
  PartialEthTransaction -> TransactionID -> FaethWatcherM TransactionID
processEthTX PartialEthTransaction{..} = do
  guardFee ethValue
  decodeAndQueue ethTXID ethTXData
  where guardFee _ = return () -- For now

decodeAndQueue :: Hex -> FaeTX -> TransactionID -> FaethWatcherM TransactionID
decodeAndQueue ethTXID (FaeTX txMessage mainFile0 modules0) lastTXID = 
  lift $ handleAll ethTXError $ do
    let 
      tx = maybe (error "Invalid transaction message") id $ 
        txMessageToTX txMessage
      thisTXID = txID tx
      mainFile = addHeader thisTXID mainFile0
      modules = Map.mapWithKey (fixHeader thisTXID) modules0
    resultVar <- ioAtomically newEmptyTMVar
    callerTID <- view _1
    txResult <- handleAll (execError thisTXID) $ 
      waitRunTXExecData queueTXExecData
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
    ethTXError e = do
      liftIO . putStrLn $
        "Error while processing Ethereum transaction " ++ show ethTXID ++
        "\nError was: " ++ show e
      return lastTXID
    execError txID e = return $
      "Error while executing Fae transaction " ++ show txID ++
      "in Ethereum transaction " ++ show ethTXID ++
      "\nError was: " ++ show e

runFaethWatcherM :: ThreadId -> FaethWatcherM () -> FaethM ()
runFaethWatcherM tID xFW = do
  subIDE <- sendReceiveProtocolT ParitySubscribe
  case subIDE of
    Left e -> error $ 
      "Ethereum client returned an error: " ++ show e
    Right subID -> 
      flip runReaderT (tID, subID) $ flip evalStateT Map.empty $ evalContT xFW

receiveSubscription :: FaethWatcherM PartialEthBlock
receiveSubscription = do
  EthNewBlock{..} <- receiveRequest "parity_subscription"
  subID <- view _2
  unless (ethSubID == subID) $ error $
    "parity_subscribe notification has subscription ID " ++ show ethSubID ++
    "; expected " ++ show subID
  either (error . errMessage) return ethBlockE


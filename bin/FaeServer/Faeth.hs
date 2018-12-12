{- |
Module: FaeServer.Faeth
Description: A protocol for using Ethereum transactions to carry Fae ones
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae does not have an associated blockchain; it is, rather, compatible with
any consensus mechanism whatsoever that creates a transaction ordering.
This module (along with "Common.ProtocolT") implements such an integration
with Ethereum, by hiding the Fae transaction inside the data field of the
Ethereum one, which is not interpreted.  

Various Ethereum parameters can be mandated by the Fae transaction's
extensible "salt" field, so that the Fae history can reflect a precise
relationship with the Ethereum history; for example, that certain ether
payments were made in exchange for performing a Fae transaction, or that a particular Ethereum contract was called with specific arguments.

The mechanism used in this module is to communicate with an Ethereum client
(Parity) via JSON-RPC calls to watch for new blocks, submit new
transactions, or request old ones.
-}

module FaeServer.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import FaeServer.Args

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
import Data.Proxy

import FaeServer.App
import FaeServer.Concurrency
import FaeServer.Modules

import System.IO

-- | The JSON-RPC structure specific to a new block notification.
data EthNewBlock =
  EthNewBlock
  {
    ethSubID :: Hex,
    ethBlockE :: Either Error PartialEthBlock
  }

-- | The structure actually describing the portions of a block that are
-- relevant to Faeth (omitting, for example, the gas limit).
data PartialEthBlock =
  PartialEthBlock
  {
    ethBlockNumber :: HexInteger,
    ethBlockHash :: EthBlockID,
    ethParentHash :: EthBlockID,
    ethBlockTXs :: [Deferred PartialEthTransaction]
  }

-- | A semantically meaningful way of saying "there may or may not be
-- something here, but we will not figure it out right now", i.e. a lazy 'Maybe'.
newtype Deferred a = Deferred { getDeferred :: Maybe a }

-- | Similar to 'PartialEthBlock', this is the Fae-relevant portion of
-- a JSON-RPC transaction object.  Each of the fields corresponds to
-- a constraint that the Fae transaction's salt may impose.
data PartialEthTransaction =
  PartialEthTransaction
  {
    ethTXTo :: EthAddress,
    ethTXFrom :: EthAddress,
    ethValue :: HexInteger,
    ethTXData :: EthArgFaeTX,
    ethTXID :: EthTXID 
  }

-- | Token for the JSON-RPC call to use the @parity.subscribe@ nonstandard
-- method.  This is the main reason that Faeth requires Parity.
data ParitySubscribe = ParitySubscribe
-- | Token for the JSON-RPC call to get a specific block.
data EthGetBlockByHash = EthGetBlockByHash EthBlockID

-- | We maintain a record of what Fae transaction was at the end of each
-- Ethereum block; this way, if a new block comes in that has an old
-- parent block, we can roll back Fae to the associated parent transaction
-- at the end of that block.
type BlockLastTXs = Map EthBlockID TransactionID
-- | The monad in which the Faeth event loop takes place, piling on several
-- effects:
--   - 'TXQueueT', so that transactions can be submitted to the interpreter
--   - 'ProtocolT', so that JSON-RPC calls can be exchanged
--   - 'ReaderT' for remembering the current thread's ID, the Parity
--     subscription's ID, and the block-transaction mapping, which is
--     actually mutable because it is stored in a 'TVar'.
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

-- | Reflects the fact that the block-transaction mapping in
-- 'FaethWatcherM' is actually mutable state.
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

-- | -
instance ToJSON ParitySubscribe where
  toJSON ParitySubscribe = 
    toJSON 
    [
      "eth_getBlockByNumber", 
      toJSON ["latest", toJSON True]
    ]

-- | -
instance ToJSON EthGetBlockByHash where
  toJSON (EthGetBlockByHash hash) = toJSON [toJSON hash, toJSON True]

-- | This tries, first, to decode an 'a'.  If that fails, it returns
-- 'Nothing', so the question of what to do with the contents of the
-- block's transaction list is /deferred/ until we have the information to
-- deal with it, while at the same time allowing the message to be decoded
-- at all.
instance (FromJSON a) => FromJSON (Deferred a) where
  parseJSON x = (Deferred . Just <$> parseJSON x) <|> return (Deferred Nothing)

-- | -
instance FromJSON EthNewBlock where
  parseJSON = A.withObject "EthNewHead" $ \obj -> do
    ethSubID <- obj .: "subscription"
    errorM <- obj .:? "error"
    resultM <- obj .:? "result"
    let
      errorE = Left $ fromMaybe (error "Invalid response") errorM
      ethBlockE = maybe errorE Right resultM
    return EthNewBlock{..}

-- | -
instance FromJSON PartialEthBlock where
  parseJSON = A.withObject "PartialEthBlock" $ \obj ->
    PartialEthBlock 
    <$> obj .: "number"
    <*> obj .: "hash"
    <*> obj .: "parentHash"
    <*> obj .: "transactions"
    
-- | -
instance FromJSON PartialEthTransaction where
  parseJSON = A.withObject "PartialEthTransaction" $ \obj ->
    PartialEthTransaction
    <$> obj .: "to"
    <*> obj .: "from"
    <*> obj .: "value"
    <*> obj .: "input"
    <*> obj .: "hash"

-- | -
instance ToRequest EthNewBlock where
  requestMethod = const "parity_subscription"

-- | -
instance ToRequest ParitySubscribe where
  requestMethod = const "parity_subscribe"

-- | -
instance ToRequest EthGetBlockByHash where
  requestMethod = const "eth_getBlockByHash"

-- | In addition to running a watcher thread, this also fires up
-- a normal-mode Fae server, except with a variant 'SendTXExecData'.
runFaeth :: ServerArgs -> ThreadId -> TXQueueT IO ()
runFaeth args@ServerArgs{..} mainTID = reThrow mainTID $ do
  fork $ runFaethWatcherM args faethWatcher 
  runServer faePort (serverApp $ Proxy @Salt) faethSendTXExecData 

-- | Faeth's Fae server does not directly execute real transactions, so any
-- that come in are coerced to be @fake@.
faethSendTXExecData :: SendTXExecData (TXQueueT IO) 
faethSendTXExecData txED@TXExecData{} = queueTXExecData txED{fake = True}
faethSendTXExecData txED = queueTXExecData txED

-- | The main event loop, this repeatedly blocks on Parity reporting a new
-- block, then scans through its transactions to find the Fae ones.
faethWatcher :: FaethWatcherM ()
faethWatcher = forever $ receiveSubscription >>= processNewBlock

-- | If the block is genuinely new (unknown to the block-transaction
-- mapping) we add it; otherwise, we skip it.
--
-- It is a little mysterious why the return type is 'TransactionID' given
-- that 'faethWatcher' buries it in a 'forever' that discards the return
-- value.  In fact, this is only because 'addNewBlock' returns
-- a transaction ID, and this in turn is because this is used in the
-- recursion through its ancestors.
processNewBlock :: PartialEthBlock -> FaethWatcherM TransactionID
processNewBlock b@PartialEthBlock{ethBlockHash} = do
  thisBlockHashM <- use $ at ethBlockHash
  maybe (addNewBlock b) return thisBlockHashM

-- | If necessary, recursively gets the parent of the new block to find the
-- Fae transaction to use as the Fae parent, then finalizes the deferral of
-- the decoded transaction list and adds the ones where the decoding
-- succeeded in producing a Fae transaction.
addNewBlock :: PartialEthBlock -> FaethWatcherM TransactionID
addNewBlock b@PartialEthBlock{..} = do
  lastTXIDM <- use $ at ethParentHash
  lastTXID <- maybe (recurseBlock b) return lastTXIDM
  thisTXID <- processEthTXs (mapMaybe getDeferred ethBlockTXs) lastTXID
  modify $ Map.insert ethBlockHash thisTXID
  return thisTXID

-- | Specifically requests the parent block and tries to add it (which may
-- trigger another recursion into /its/ parent).
recurseBlock :: PartialEthBlock -> FaethWatcherM TransactionID
recurseBlock PartialEthBlock{..}
  | ethBlockNumber == 0 = return nullID
  | otherwise = do
      parentBlock <- sendReceiveProtocolT (EthGetBlockByHash ethParentHash)
      addNewBlock parentBlock

-- | Folds the list of transactions into the interpreter until the last ID
-- is found.
processEthTXs :: 
  [PartialEthTransaction] -> TransactionID -> FaethWatcherM TransactionID
processEthTXs ethBlockTXs lastTXID = do
  foldl (>>=) (return lastTXID) $ 
    map processEthTX ethBlockTXs

-- | Validates the constraints on the Ethereum metadata contained in the
-- Fae transaction's salt, then sends the Fae transaction to the
-- interpreter.
processEthTX :: 
  PartialEthTransaction -> TransactionID -> FaethWatcherM TransactionID
processEthTX PartialEthTransaction{..} lastTXID = handleAll ethTXError $ do
  case (ethTXTo ==) <$> recipM of
    Just False -> error $
      "Incorrect recipient: expected " ++ show (fromJust recipM) ++
      "; got " ++ show ethTXTo
    _ -> return ()
  case (ethValue >=) <$> feeM of
    Just False -> error $
      "Insufficient Ether provided: " ++ 
      "needed " ++ show (fromJust feeM) ++ "; got " ++ show ethValue
    _ -> return ()
  runFaethTX ethTXID (getEthArgFaeTX ethTXData) lastTXID

  where
    theSalt = salt . faeTXMessage . getEthArgFaeTX $ ethTXData
    recipM = ethRecipient theSalt
    feeM = ethFee theSalt
    ethTXError e = do
      liftIO . putStrLn $
        "\nError while processing Ethereum transaction " ++ show ethTXID ++
        "\nError was: " ++ show e ++ "\n"
      return lastTXID

-- | The Faeth analogue of 'Blockchain.Fae.App.serverApp', this constructs
-- a 'TXExecData' and runs it.  Unlike that app, it is not expected to run
-- in parallel with other instances; this function blocks entirely until
-- the transaction is finished being interpreted, since there is nothing to
-- do with the results until they are all done anyway.
runFaethTX :: Hex -> FaeTX -> TransactionID -> FaethWatcherM TransactionID
runFaethTX ethTXID (FaeTX txMessage mainFile0 modules0) lastTXID = do
  let
    (tx, mainFile, modules) = 
      makeFilesMap txMessage mainFile0 modules0 False False
    thisTXID = txID tx
    execError txID e = liftIO . putStrLn $
      "\nError while executing Fae transaction " ++ show txID ++
      "\n              in Ethereum transaction " ++ show ethTXID ++
      "\nError was: " ++ show e ++ "\n"
  tmVar <- ioAtomically newEmptyTMVar
  callerTID <- view _1
  handleAll (execError thisTXID) $ do
    txResult <- waitRunTXExecData queueTXExecData resultVar
      TXExecData
      {
        parentM = Just lastTXID,
        lazy = True,
        fake = False,
        resultVar = tmVar,
        ..
      }
    liftIO $ putStrLn txResult
  return thisTXID

-- | Initializes the monad; crucially, this restarts on an uncaught
-- exception rather than crashing the Faeth thread.  The subscription is
-- set up and the action is run with knowledge of it.
runFaethWatcherM :: ServerArgs -> FaethWatcherM () -> TXQueueT IO ()
runFaethWatcherM ServerArgs{..} xFW = do
  tID <- myThreadId
  blockTXIDs <- liftIO $ newTVarIO Map.empty
  forever $ handleAll waitRestart $ 
    runProtocolT faethHostname faethPort $ do
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

-- | Abstracts away the protocol for receiving a subscription update from
-- Parity.  Subscription updates are a little weird compared to other
-- JSON-RPC responses since they actually come in the form of a /request/
-- (I guess a "response" can only be sent after receiving an unresolved
-- request, while requests can be sent unsolicited).
receiveSubscription :: FaethWatcherM PartialEthBlock
receiveSubscription = do
  EthNewBlock{..} <- receiveRequest "parity_subscription"
  subID <- view _2
  unless (ethSubID == subID) $ error $
    "parity_subscribe notification has subscription ID " ++ show ethSubID ++
    "; expected " ++ show subID
  either (error . errMessage) return ethBlockE

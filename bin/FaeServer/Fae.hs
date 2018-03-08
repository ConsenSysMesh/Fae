module FaeServer.Fae where

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class

import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Map as Map

import FaeServer.Concurrency
import FaeServer.Git
import FaeServer.History
import FaeServer.Modules

import System.Directory
import System.Exit

runFae :: TXQueue -> ThreadId -> IO ()
runFae txQueue mainTID = reThrow mainTID $ runFaeInterpretWithHistory $ 
  forever $ do
    txExecData <- ioAtomically $ readTQueue txQueue
    reThrowExit mainTID (callerTID txExecData) $ runTXExecData mainTID txExecData

runTXExecData :: ThreadId -> TXExecData -> FaeInterpretWithHistory ()
runTXExecData mainTID TXExecData{tx=tx@TX{..}, ..} = do
  dup <- gets $ Map.member txID . txStorageAndCounts
  when dup $ error $ "Duplicate transaction ID: " ++ show txID

  txCount <- recallHistory parentM
  liftIO $ writeModules mainFile modules txID
  txResult <- lift $ do
    interpretTX reward tx
    if lazy
    then return $ "Transaction " ++ show txID
    else lift $ showTransaction txID
  if fake
  then liftIO gitClean
  else updateHistory txID txCount
  ioAtomically $ putTMVar resultVar txResult

runTXExecData mainTID View{..} = do
  void $ recallHistory parentM
  txResult <- lift $ lift $ showTransaction txID
  ioAtomically $ putTMVar resultVar txResult

reThrowExit :: (MonadIO m, MonadCatch m) => ThreadId -> ThreadId -> m () -> m ()
reThrowExit mainTID callerTID =
  reThrow callerTID . handle (liftIO . throwTo @ExitCode mainTID)

reThrow :: (MonadIO m, MonadCatch m) => ThreadId -> m () -> m ()
reThrow tID = handleAll (liftIO . throwTo tID)

ioAtomically :: (MonadIO m) => STM a -> m a
ioAtomically = liftIO . atomically

nextTX :: [(String, String)] -> Inputs -> [String] -> IO TX
nextTX keyNames inputs fallback = do
  signerNonces <- forM keyNames $ \(signer, keyName) -> do
    keyExists <- doesFileExist keyName
    unless keyExists $ do
      privKey <- newPrivateKey
      B.writeFile keyName $ S.encode (privKey, 0 :: Int) 
    decodeE <- S.decode <$> B.readFile keyName
    let (privKey, nonce :: Int) = either error id decodeE
    B.writeFile keyName $ S.encode (privKey, nonce + 1)
    let Just pubKey = public privKey
    return (signer, (pubKey, nonce))
  let 
    pubKeys = Signers $ fmap fst $ Map.fromList signerNonces
    txID = ShortContractID $ digest signerNonces
  return TX{..}




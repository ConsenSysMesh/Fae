{- |
Module: FaeServer.App
Description: The HTTP-handling part of faeServer, based on Warp
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

We use the Warp Haskell webserver to handle the HTTP protocol.  In fact, there are several servers defined here: one for normal mode that accepts transactions, and one for import/export mode that transfers contract call results.
-}
module FaeServer.App where

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Proxy
import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import FaeServer.Concurrency
import FaeServer.Fae
import FaeServer.Modules

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Network.Wai.Parse

import Text.Read (readMaybe)

-- | It is necessary for us to operate in a monad above 'IO' in our
-- servers, and this encapsulates that requirement.
type ApplicationT m = 
  Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
-- | Builds an application type based on a particular method of sending
-- transactions to the interpreter.
type TXExecApplicationT m = SendTXExecData m -> ApplicationT m
-- | Uses the basic transaction-sending monad 'TXQueueT' that places
-- incoming transactions in a 'TMQueue'.
type TXExecApplication = TXExecApplicationT (TXQueueT IO)

-- | Handles the issue of converting an 'ApplicationT' to an 'Application',
-- which is what Warp actually uses, then runs it.
runServer :: 
  Int -> TXExecApplication -> SendTXExecData (TXQueueT IO) -> TXQueueT IO ()
runServer port makeApp sendTXExecData = do
  app <- bringOut $ makeApp sendTXExecData 
  liftIO $ runSettings (faeSettings port) app

-- | These settings just specify the port and the method of reporting
-- exceptions.  It is possible that these are biased towards testing usage,
-- and should be more nuanced.
faeSettings :: Int -> Settings
faeSettings port = defaultSettings &
  setPort port &
  setOnExceptionResponse exceptionResponse

-- | The normal-mode server: accepts the full request object described in
-- the usage and crafts an instruction to run or view a transaction based
-- on it.
serverApp :: forall a. (Serialize a) => Proxy a -> TXExecApplication
serverApp _ sendTXExecData = \request respond -> do
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request
  let 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent" 
    viewM = getLast Nothing Just $ getParams "view" 
    lazy = getLast False id $ getParams "lazy" 
    fake = getLast False id $ getParams "fake" 
    reward = getLast False id $ getParams "reward" 

  let send = waitResponse respond stringHeaders stringUtf8 sendTXExecData resultVar
  case viewM of
    Just viewTXID 
      | fake -> error "'fake' and 'view' are incompatible parameters"
      | lazy -> error "'lazy and 'view' are incompatible parameters"
      | otherwise -> send $ \callerTID resultVar -> View{..}
    Nothing -> 
      let mainFile0 = getFile files "body"
          modules0 = Map.fromList $ getFiles files "other"
          txMessage = 
            either (error "Couldn't decode transaction message") id $ 
            S.decode @(TXMessage a) $ getFile files "message"
          (tx, mainFile, modules) =
            makeFilesMap txMessage mainFile0 modules0 reward fake
      in  send $ \callerTID resultVar -> TXExecData{..} 

-- | The import/export-mode app, which runs alongside the normal one even
-- in Faeth mode.  This accepts exactly one of "import" and "export"
-- parameters as well as others, described in the usage.
importExportApp :: TXExecApplication
importExportApp sendTXExecData = \request respond -> do
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request
  let
    send :: 
      (Serialize a) => 
      (TXExecData -> TMVar a) ->
      (ThreadId -> TMVar a -> TXExecData) -> 
      TXQueueT IO ResponseReceived
    send = waitResponse respond dataHeaders (byteString . S.encode) sendTXExecData 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent"
    importDataM = either error id . S.decode <$> getFileMaybe files "import"
    exportDataM = either error id . S.decode <$> getFileMaybe files "export"
  case (importDataM, exportDataM) of
    (Just (exportedCID, exportStatus, neededModules, exportValType), Nothing) ->
      let exportedValue = getFile files "valuePackage"
          exportData = ExportData{..}
      in send signalVar $ \callerTID signalVar -> ImportValue{..}
    (Nothing, Just (calledInTX, ixInTX)) ->
      send exportResultVar $ \callerTID exportResultVar -> ExportValue{..}
    (Nothing, Nothing) -> 
        error "Must specify either 'import' or 'export' parameter"
    _ -> error "Can't specify both 'import' and 'export' parameters"

-- | This magical function converts an 'ApplicationT' to an 'Application'
-- inside its monad.  This is actually a specific case of
-- @MonadBaseControl@ that only applies to 'ReaderT'.
bringOut :: ApplicationT (ReaderT r IO) -> ReaderT r IO Application
bringOut txqApp =
  ReaderT $ \txq -> return $ \req resp -> runReaderT (txqApp req resp) txq

-- | A higher-order abstraction that sends an instruction to the
-- interpreter queue, handling the concurrency, and then building and
-- replying with a response based on its result.
waitResponse :: 
  (TXQueueM m) =>
  (Response -> IO ResponseReceived) -> 
  ResponseHeaders ->
  (a -> Builder) ->
  SendTXExecData m -> 
  (TXExecData -> TMVar a) ->
  (ThreadId -> TMVar a -> TXExecData) -> 
  m ResponseReceived
waitResponse respond headers build sendTXExecData tmVarField constr = do
  callerTID <- liftIO myThreadId
  resultVar <- ioAtomically newEmptyTMVar
  let txExecData = constr callerTID resultVar
  result <- waitRunTXExecData sendTXExecData tmVarField txExecData
  liftIO $ respond $ responseBuilder ok200 headers $ build result

-- | A query parameter may be specified multiple times; this filters out
-- only the ones for a specific parameter from the list of all those
-- passed, and also deserializes them (potentially unnecessary, actually,
-- since aside from 'String's they can likely be 'S.decode'd directly).
getParameters :: [(C8.ByteString, C8.ByteString)] -> C8.ByteString -> [String]
getParameters params paramName = 
  [ C8.unpack s | (pName, s) <- params, pName == paramName]

-- | A multipurpose tool for getting and using the last, and therefore
-- effective, value of a query parameter.
getLast :: (Read b) => a -> (b -> a) -> [String] -> a
getLast x0 f l = last $ x0 : map (f . readMay) l where
  readMay s = fromMaybe (error $ "Couldn't read query parameter value: " ++ s) $
    readMaybe s

-- | When the app throws an exception that it doesn't catch, Warp catches
-- it, and this function builds a simple response with what I think are
-- appropriate response code and headers.  Nonetheless, something may be
-- wrong, as non-ascii special characters contained in GHC error messages
-- are garbled when printed by 'postTX'.
exceptionResponse :: SomeException -> Response
exceptionResponse = responseBuilder badRequest400 stringHeaders . stringUtf8 . show

-- | All Haskell text is utf8 by default.
stringHeaders :: ResponseHeaders
stringHeaders = 
  [
    (hContentEncoding, "utf8"),
    (hContentType, "text/plain")
  ]

-- | Data is an octet-stream.
dataHeaders :: ResponseHeaders
dataHeaders =
  [
    (hContentType, "application/octet-stream")
  ]

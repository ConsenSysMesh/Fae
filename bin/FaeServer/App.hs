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

type ApplicationT m = 
  Request -> (Response -> IO ResponseReceived) -> m ResponseReceived
type TXExecApplicationT m = SendTXExecData m -> ApplicationT m
type TXExecApplication = TXExecApplicationT (TXQueueT IO)

runServer :: 
  Int -> TXExecApplication -> SendTXExecData (TXQueueT IO) -> TXQueueT IO ()
runServer port makeApp sendTXExecData = do
  app <- bringOut $ makeApp sendTXExecData 
  liftIO $ runSettings (faeSettings port) app

faeSettings :: Int -> Settings
faeSettings port = defaultSettings &
  setPort port &
  setOnExceptionResponse exceptionResponse

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
      let (tx, mainFile, modules) = makeFilesMap (Proxy @a) files reward
      in  send $ \callerTID resultVar -> TXExecData{..} 

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
    importDataM = getLast Nothing Just $ getParams "import"
    exportDataM = getLast Nothing Just $ getParams "export"
    parentM = getLast Nothing Just $ getParams "parent"
  case (importDataM, exportDataM) of
    (Just (importedCID, valueType), Nothing) ->
      let valuePackage = fromMaybe (error $ "Missing 'valuePackage' file") $
            getFile files "valuePackage"
      in send signalVar $ \callerTID signalVar -> ImportValue{..}
    (Nothing, Just (calledInTX, shortCID)) ->
      send exportResultVar $ \callerTID exportResultVar -> ExportValue{..}
    (Nothing, Nothing) -> 
        error "Must specify either 'import' or 'export' parameter"
    _ -> error "Can't specify both 'import' and 'export' parameters"

bringOut :: ApplicationT (ReaderT r IO) -> ReaderT r IO Application
bringOut txqApp =
  ReaderT $ \txq -> return $ \req resp -> runReaderT (txqApp req resp) txq

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

getParameters :: [(C8.ByteString, C8.ByteString)] -> C8.ByteString -> [String]
getParameters params paramName = 
  [ C8.unpack s | (pName, s) <- params, pName == paramName]

getLast :: (Read b) => a -> (b -> a) -> [String] -> a
getLast x0 f l = last $ x0 : map (f . read) l

exceptionResponse :: SomeException -> Response
exceptionResponse = responseBuilder badRequest400 stringHeaders . stringUtf8 . show

stringHeaders :: ResponseHeaders
stringHeaders = 
  [
    (hContentEncoding, "utf8"),
    (hContentType, "text/plain")
  ]

dataHeaders :: ResponseHeaders
dataHeaders =
  [
    (hContentType, "application/octet-stream")
  ]

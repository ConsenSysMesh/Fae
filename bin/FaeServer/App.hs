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

runFaeServer :: 
  forall a. (Serialize a) => Proxy a ->
  SendTXExecData (TXQueueT IO) -> TXQueueT IO ()
runFaeServer p sendTXExecData = do
  app <- serverApp p sendTXExecData 
  liftIO $ runSettings faeSettings app

faeSettings :: Settings
faeSettings = defaultSettings &
  setPort 27182 &
  setOnExceptionResponse exceptionResponse

serverApp :: 
  forall a. (Serialize a) => Proxy a ->
  SendTXExecData (TXQueueT IO) -> TXQueueT IO Application
serverApp _ sendTXExecData = bringOut $ \request respond -> do
  (params, files) <- liftIO $ parseRequestBody lbsBackEnd request
  let 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent" 
    viewM = getLast Nothing Just $ getParams "view" 
    lazy = getLast False id $ getParams "lazy" 
    fake = getLast False id $ getParams "fake" 
    reward = getLast False id $ getParams "reward" 

  let send = waitResponse respond sendTXExecData
  case viewM of
    Just viewTXID 
      | fake -> error "'fake' and 'view' are incompatible parameters"
      | lazy -> error "'lazy and 'view' are incompatible parameters"
      | otherwise -> send $ \callerTID resultVar -> View{..}
    Nothing -> 
      let (tx, mainFile, modules) = makeFilesMap (Proxy @a) files
      in  send $ \callerTID resultVar -> TXExecData{..} 

  where
    bringOut txqApp =
      ReaderT $ \txq -> return $ \req resp -> runReaderT (txqApp req resp) txq

waitResponse :: 
  (TXQueueM m) =>
  (Response -> IO ResponseReceived) -> 
  SendTXExecData m -> 
  (ThreadId -> TMVar String -> TXExecData) -> 
  m ResponseReceived
waitResponse respond sendTXExecData constr = do
  callerTID <- liftIO myThreadId
  resultVar <- ioAtomically newEmptyTMVar
  let txExecData = constr callerTID resultVar
  result <- waitRunTXExecData sendTXExecData txExecData
  liftIO $ respond $ buildResponse result

getParameters :: [(C8.ByteString, C8.ByteString)] -> C8.ByteString -> [String]
getParameters params paramName = 
  [ C8.unpack s | (pName, s) <- params, pName == paramName]

getLast :: (Read b) => a -> (b -> a) -> [String] -> a
getLast x0 f l = last $ x0 : map (f . read) l

buildResponse :: String -> Response
buildResponse = responseBuilder ok200 headers . stringUtf8 

exceptionResponse :: SomeException -> Response
exceptionResponse = responseBuilder badRequest400 headers . stringUtf8 . show

headers :: ResponseHeaders
headers = 
  [
    (hContentEncoding, "utf8"),
    (hContentType, "text/plain")
  ]


module FaeServer.App where

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as Map
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

faeSettings :: Settings
faeSettings = defaultSettings &
  setPort 27182 &
  setOnExceptionResponse exceptionResponse

serverApp :: TXQueue -> Application
serverApp txQueue request respond = do
  (params, files) <- parseRequestBody lbsBackEnd request
  let 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent" 
    viewM = getLast Nothing Just $ getParams "view" 
    lazy = getLast False id $ getParams "lazy" 
    fake = getLast False id $ getParams "fake" 
    reward = getLast False id $ getParams "reward" 

  case viewM of
    Just txID 
      | fake -> error "'fake' and 'view' are incompatible parameters"
      | lazy -> error "'lazy and 'view' are incompatible parameters"
      | otherwise -> send $ \callerTID resultVar -> View{..}
    Nothing -> 
      let (tx, mainFile, modules) = makeFilesMap files
      in  send $ \callerTID resultVar -> TXExecData{..} 

  where send = sendTXExecData respond txQueue

sendTXExecData :: 
  (Response -> IO ResponseReceived) -> 
  TXQueue -> 
  (ThreadId -> TMVar String -> TXExecData) -> 
  IO ResponseReceived
sendTXExecData respond txQueue constr = do
  callerTID <- myThreadId
  resultVar <- atomically newEmptyTMVar
  let txExecData = constr callerTID resultVar
  atomically $ writeTQueue txQueue txExecData
  result <- atomically $ takeTMVar resultVar
  respond $ buildResponse result

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


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
  setOnExceptionResponse exceptionResponseForDebug

serverApp :: TXQueue -> Application
serverApp txQueue request respond = do
  (params, files) <- parseRequestBody lbsBackEnd request
  let 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent" 
    fake = getLast False id $ getParams "fake" 
    reward = getLast False id $ getParams "reward"
    keyNames = if null keyNames0 then [("self", "key1")] else keyNames0 where
      keyNames0 = map uncolon $ getParams "key"
      uncolon s = (x, tail y) where (x, y) = break (== ':') s
    inputs = fromMaybe inputsErr $ mapM readMaybe $ getParams "input" where
      inputsErr = error "Couldn't parse inputs"
    fallback = getParams "fallback"

  tx@TX{pubKeys, txID} <- nextTX keyNames inputs fallback >>= evaluate . force
  let (mainFileM, modules) = makeFilesMap files txID
  case mainFileM of
    Nothing -> respond $ buildResponse $ 
      intercalate "\n" $ 
      map (\(x,y) -> x ++ ": " ++ show y) $
      Map.toList $
      getSigners pubKeys
    Just mainFile -> do
      callerTID <- myThreadId
      resultVar <- atomically newEmptyTMVar
      atomically $ writeTQueue txQueue TXExecData{..}
      result <- atomically $ takeTMVar resultVar
      respond $ buildResponse result

getParameters :: [(C8.ByteString, C8.ByteString)] -> C8.ByteString -> [String]
getParameters params paramName = 
  [ C8.unpack s | (pName, s) <- params, pName == paramName]

getLast :: (Read b) => a -> (b -> a) -> [String] -> a
getLast x0 f l = last $ x0 : map (f . read) l

buildResponse :: String -> Response
buildResponse = responseBuilder ok200 headers . stringUtf8 where
  headers = 
    [
      (hContentEncoding, "utf8"),
      (hContentType, "text/plain")
    ]


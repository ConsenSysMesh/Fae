module PostTX.Network where

import Data.Maybe
import Data.Serialize (Serialize)

import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S

import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.TXSpec

requestURL :: String -> Request
requestURL host = fromMaybe (error $ "Bad host string: " ++ host) $ 
  parseRequest $ "http://" ++ host
    
sendReceive :: (LC8.ByteString -> a) -> Request -> IO a
sendReceive decode request = do
  manager <- newManager defaultManagerSettings
  response <- httpLbs request manager 
  return $ decode $ responseBody response

sendReceiveString :: Request -> IO String
sendReceiveString = sendReceive LC8.unpack

sendReceiveSerialize :: (Serialize a) => Request -> IO a
sendReceiveSerialize = 
  sendReceive $ either (error "Invalid response") id . S.decodeLazy

modulePart :: String -> String -> Module -> Part
modulePart param name = partFileRequestBody (T.pack param) name . RequestBodyBS 

moduleParts :: String -> Modules -> [Part]
moduleParts param = Map.foldrWithKey (\name -> (:) . modulePart param name) []


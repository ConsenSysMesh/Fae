module PostTX.Network where

import Blockchain.Fae.FrontEnd

import Common.JSON()

import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe
import Data.Proxy
import Data.Serialize (Serialize)

import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S

import qualified Data.Text as T
import Data.Typeable

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData

import PostTX.TXSpec

import Text.PrettyPrint.HughesPJClass
  
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

sendReceiveSerialize :: (Typeable a, Serialize a) => Request -> IO a
sendReceiveSerialize = 
  sendReceive $ \bs -> either (error $ LC8.unpack bs) id $ S.decodeLazy bs

sendReceiveJSON :: (Typeable a, FromJSON a) => Request -> IO (Either String a)
sendReceiveJSON = sendReceive $ \bs ->
  bimap (\_ -> LC8.unpack bs) id $ A.eitherDecode bs

sendReceiveJSONString :: Bool -> Request -> IO String
sendReceiveJSONString isJson
  | isJson    = sendReceiveString
  | otherwise = fmap (either id prettyShow) . sendReceiveJSON @TXSummary

responseError :: forall a. (Typeable a) => String -> a
responseError s = 
  error $ "Couldn't parse response as type " ++ show tr ++ ": " ++ s
  where tr = typeRep $ Proxy @a

modulePart :: String -> String -> Module -> Part
modulePart param name = partFileRequestBody (T.pack param) name . RequestBodyBS 

moduleParts :: String -> Modules -> [Part]
moduleParts param = Map.foldrWithKey (\name -> (:) . modulePart param name) []

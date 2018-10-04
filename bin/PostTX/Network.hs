{- |
Module: PostTX.Network
Description: Handler for postTX's Faeth mode
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The foundational module for all network-using modes, abstracting away the
process of connecting to the server and interpreting its response.
-}

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
import qualified Data.Text.Encoding as T
import Data.Typeable

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData

import PostTX.TXSpec

import Text.PrettyPrint.HughesPJClass

-- | Makes a request from an HTTP URL (here, just the hostname).
requestURL :: String -> Request
requestURL host = fromMaybe (error $ "Bad host string: " ++ host) $ 
  parseRequest $ "http://" ++ host
    
-- | Performs the conversation with the server and handles the response as
-- requested.
sendReceive :: (LC8.ByteString -> a) -> Request -> IO a
sendReceive decode request = do
  manager <- newManager defaultManagerSettings
  response <- httpLbs request manager 
  return $ decode $ responseBody response

-- | Specialization of 'sendReceive' that expects a 'String' response
-- literally represented in UTF-8 format by the response bytestring.
sendReceiveString :: Request -> IO String
sendReceiveString = sendReceive LC8.unpack

-- | Specialization of 'sendReceive' that expects a typed value encoded as
-- the response bytestring.
sendReceiveSerialize :: (Typeable a, Serialize a) => Request -> IO a
sendReceiveSerialize = 
  sendReceive $ \bs -> either (error $ LC8.unpack bs) id $ S.decodeLazy bs

-- | Specialization of 'sendReceive' that expects a JSON value encoded as
-- the response bytestring.
sendReceiveJSON :: (Typeable a, FromJSON a) => Request -> IO (Either String a)
sendReceiveJSON = sendReceive $ \bs ->
  bimap (\_ -> T.unpack $ T.decodeUtf8 $ LC8.toStrict bs) id $ A.eitherDecode bs

-- | Combines the 'String' and 'JSON' variants, pretty-printing the latter
-- if it is requested.
sendReceiveJSONString :: Bool -> Request -> IO String
sendReceiveJSONString isJson
  | isJson    = sendReceiveString
  | otherwise = fmap (either id prettyShow) . sendReceiveJSON @TXSummary

-- | Standard error format for this module.
responseError :: forall a. (Typeable a) => String -> a
responseError s = 
  error $ "Couldn't parse response as type " ++ show tr ++ ": " ++ s
  where tr = typeRep $ Proxy @a

-- | Creates a request part containing a module source code file.
modulePart :: String -> String -> Module -> Part
modulePart param name = partFileRequestBody (T.pack param) name . RequestBodyBS 

-- | Creates all parts from a module-name mapping.
moduleParts :: String -> Modules -> [Part]
moduleParts param = Map.foldrWithKey (\name -> (:) . modulePart param name) []


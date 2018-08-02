module PostTX.Submit where

import qualified Data.ByteString.Char8 as C8

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S
import Data.Serialize (Serialize)

import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.Network
import PostTX.TXSpec

submit :: String -> String -> Bool -> Bool -> Bool -> TXSpec String -> IO ()
submit txName host fake lazy isJson txSpec = 
  buildRequest txName host fake lazy txSpec >>= sendReceive isJson

buildRequest :: String -> String -> Bool -> Bool -> TXSpec String -> IO Request
buildRequest txName host fake lazy TXSpec{specModules = LoadedModules{..}, ..} =
  flip formDataBody (requestURL host) $
    modulePart "message" txName (S.encode txMessage) : 
    uncurry (modulePart "body") mainModule :
    moduleParts "other" otherModules ++
    fmap (uncurry partBS) (maybe id (:) parentArg [lazyArg, fakeArg, rewardArg])

  where
    lazyArg = ("lazy", ) $ if lazy then "True" else "False"
    fakeArg = ("fake", ) $ if fake then "True" else "False"
    rewardArg = ("reward", ) $ if isReward then "True" else "False"
    parentArg = ("parent", ) . C8.pack . show <$> parentM

modulePart :: String -> String -> Module -> Part
modulePart param name = partFileRequestBody (T.pack param) name . RequestBodyBS 

moduleParts :: String -> Modules -> [Part]
moduleParts param = Map.foldrWithKey (\name -> (:) . modulePart param name) []

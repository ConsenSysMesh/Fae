module PostTX.Submit where

import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as S
import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData

import PostTX.Network
import PostTX.TXSpec

-- | If JSON formatting is not enabled through postTX CLI flag
-- pretty print a summary of TX output.
submit :: String -> Bool -> Bool -> Bool -> TXSpec String -> IO ()
submit host fake lazy isJson txSpec = 
  buildRequest host fake lazy txSpec >>= sendReceiveJSONString isJson >>= putStrLn

buildRequest :: String -> Bool -> Bool -> TXSpec String -> IO Request
buildRequest host fake lazy TXSpec{specModules = LoadedModules{..}, ..} =
  flip formDataBody (requestURL host) $
    modulePart "message" txName (S.encode txMessage) : 
    modulePart "body" txName mainModuleBS :
    moduleParts "other" otherModules ++
    fmap (uncurry partBS) (maybe id (:) parentArg [lazyArg, fakeArg, rewardArg])

  where
    (txName, mainModuleBS) = mainModule
    lazyArg = ("lazy", ) $ if lazy then "True" else "False"
    fakeArg = ("fake", ) $ if fake then "True" else "False"
    rewardArg = ("reward", ) $ if isReward then "True" else "False"
    parentArg = ("parent", ) . C8.pack . show <$> parentM


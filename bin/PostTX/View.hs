module PostTX.View where

import PostTX.Network

import Blockchain.Fae (TransactionID)

import qualified Data.ByteString.Lazy.Char8 as LC8

import Network.HTTP.Client.MultipartFormData

view :: TransactionID -> String -> Bool -> IO ()
view txID host isJSON = do
  request <- flip formDataBody (requestURL host) $ 
    [partLBS "view" $ LC8.pack $ show txID]
  sendReceive isJSON request

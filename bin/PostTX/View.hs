module PostTX.View where

import PostTX.Network

import Blockchain.Fae.FrontEnd (TransactionID)

import qualified Data.ByteString.Lazy.Char8 as LC8

import Network.HTTP.Client.MultipartFormData

view :: TransactionID -> String -> IO ()
view txID host = do
  request <- flip formDataBody (requestURL host) $ 
    [partLBS "view" $ LC8.pack $ show txID]
  sendReceive request

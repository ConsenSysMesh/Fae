{- |
Module: PostTX.View
Description: Handler for postTX's View mode
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

View mode is trivial: it sends a single transaction ID and prints the
result that had already been obtained from running that transaction.
-}
module PostTX.View where

import PostTX.Network

import Blockchain.Fae.FrontEnd (TransactionID)

import qualified Data.ByteString.Lazy.Char8 as LC8

import Network.HTTP.Client.MultipartFormData

-- | The view request just has a "view" query parameter containing the
-- transaction ID.
view :: TransactionID -> String -> Bool -> IO ()
view txID host isJSON = do
  request <- flip formDataBody (requestURL host) 
    [partLBS "view" $ LC8.pack $ show txID]
  sendReceiveJSONString isJSON request >>= putStrLn

{- |
Module: Common.JSON
Description: Orphan JSON instances for types that are employed by both client and server
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental
-}
{-# LANGUAGE RecordWildCards #-}
module Common.JSON where 

import Blockchain.Fae.FrontEnd 

import qualified Data.Aeson as A
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D

instance ToJSON TXInputSummary where
    toJSON TXInputSummary{..} = object [
      "txInputTXID" .= show txInputTXID,
      "txInputNonce" .= show txInputNonce,
      "txInputOutputs" .= show txInputOutputs,
      "txInputVersion" .= show txInputVersion ]

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= show txResult,
    "txOutputs" .= show txOutputs,
    "txInputSummary" .=  txInputSummary,
    "signers" .= show signers ]

encodeJSON ::(ToJSON a) => a -> String
encodeJSON a = show $ X.toStrict $ D.decodeUtf8 $ A.encode a

decodeJSON :: (FromJSON a) => String -> Maybe a
decodeJSON json = A.decode $ C.pack json
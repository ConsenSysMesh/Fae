{- |
Module: Common.JSON
Description: JSON instances and Utils
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module exports JSON Orphan instances and utilities for types that 
are generally useful both to the server (@faeServer@) and the client (@postTX@).

-}
{-# LANGUAGE RecordWildCards #-}
module Common.JSON where 

import Blockchain.Fae.FrontEnd 

import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D

instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = object [
    "txInputTXID" .= show txInputTXID,
    "txInputNonce" .= show txInputNonce,
    "txInputOutputs" .= show txInputOutputs,
    "txInputVersion" .= txInputVersion ]

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= txResult,
    "txOutputs" .= show txOutputs,
    "txInputSummary" .= txInputSummary,
    "signers" .= signers ]

encodeJSON ::(ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a

decodeJSON :: (FromJSON a) => String -> Maybe a
decodeJSON json = A.decode $ C.pack json
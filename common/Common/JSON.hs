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

import qualified Data.ByteString.Char8 as C8

import Data.Aeson (eitherDecode, FromJSON, ToJSON, toJSON, parseJSON, object, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Maybe

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

instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    txInputTXID <- o .: "txInputTXID"
    txInputNonce  <- o .: "txInputNonce"
    txInputOutputs <- o .: "txInputOutputs"
    txInputVersion  <- o .: "txInputVersion"
    return TXInputSummary{..}

instance FromJSON Result where
  parseJSON (A.String result) = return $ Result result

instance ToJSON Result where
  toJSON result = A.String $ T.pack $ show result

instance FromJSON ShortContractID where
  parseJSON (A.String scid) = return $ fromMaybe (error $ errMsg ++ (show scid)) (readMaybe $ T.unpack scid)
    where errMsg = "Couldn't read ShortContractID whilst parsing TXSummary JSON: "

instance FromJSON TXSummary where
  parseJSON = withObject "TXSummary" $ \o -> do
    transactionID <- o .: "transactionID"
    txResult  <- o .: "txResult"
    txOutputs' <- o .:? "txOutputs"
    txInputSummary  <- o .: "txInputSummary"
    signers <- o .: "signers"
    let txOutputs = fromMaybe raiseErr $ readMaybe $ fromMaybe raiseErr txOutputs'
    return TXSummary{..}
    where raiseErr = error "Can't parse txOutputs when decoding TXSummary JSON"

encodeJSON ::(ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a

decodeJSON :: (FromJSON a) => LC8.ByteString -> Maybe a
decodeJSON = A.decode
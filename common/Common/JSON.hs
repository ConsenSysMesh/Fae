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
{-# LANGUAGE ScopedTypeVariables #-}
module Common.JSON where 

import Blockchain.Fae.FrontEnd

import qualified Data.ByteString.Char8 as C8

import Data.Aeson (eitherDecode, FromJSON, ToJSON, toJSON, parseJSON, object, withText, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Maybe
import Data.Typeable

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Text.Read

instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = object [
    "txInputTXID" .= show txInputTXID,
    "txInputNonce" .= show txInputNonce,
    "txInputOutputs" .= show txInputOutputs,
    "txInputVersions" .= txInputVersions ]

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= txResult,
    "txOutputs" .= show txOutputs,
    "txInputSummaries" .= txInputSummaries,
    "signers" .= signers ]

instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    TXInputSummary
      <$> o .: "txInputTXID"
      <*> (either fail return . readEither =<< (o .: "txInputNonce"))
      <*> (either fail return . readEither =<< (o .: "txInputOutputs"))
      <*> o .: "txInputVersions"

instance FromJSON PublicKey where
  parseJSON = withText "VersionID" $ \pKey -> do
    either fail return $ readEither (T.unpack pKey)

instance ToJSON PublicKey where
  toJSON vID = A.String $ T.pack $ show vID

instance FromJSON VersionID where
  parseJSON = withText "VersionID" $ \vID -> do
    either fail return $ readEither (T.unpack vID)

instance ToJSON VersionID where
  toJSON vID = A.String $ T.pack $ show vID

instance Read TypeRep where
  readsPrec = readsPrec

instance FromJSON TypeRep where
  parseJSON (A.String a) = either fail return $ readEither (T.unpack a)

instance ToJSON TypeRep where
  toJSON a = A.String $ T.pack $ show a

instance FromJSON ShortContractID where
  parseJSON = withText "ShortContractID" $ \scid -> do
    either fail return $ readEither (T.unpack scid)

instance FromJSON TXSummary where
  parseJSON = withObject "TXSummary" $ \o -> do
    transactionID <- o .: "transactionID"
    txResult  <- o .: "txResult"
    txOutputs' <- o .:? "txOutputs"
    txInputSummaries  <- o .: "txInputSummaries"
    signers <- o .: "signers"
    let txOutputs = fromMaybe raiseErr $ readMaybe $ fromMaybe raiseErr txOutputs'
    return TXSummary{..}
    where raiseErr = error "Can't parse txOutputs when decoding TXSummary JSON"
    
encodeJSON ::(ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a
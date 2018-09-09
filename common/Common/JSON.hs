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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Common.JSON where 

import Blockchain.Fae.FrontEnd

import Control.Applicative
import Control.DeepSeq

import Data.Aeson (FromJSON, ToJSON, Object, toJSON,
   parseJSON, object, Value(..), withText,
   withObject, (.=), (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Text (Text)

import System.IO.Unsafe

import Text.Read

instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = wrapExceptions $ 
    object [
      "txInputNonce" .= toJSON txInputNonce,
      "txInputOutputs" .= wrapExceptions txInputOutputs,
      "txInputVersions" .= toJSON txInputVersions ]

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= wrapExceptions txResult,
    "txOutputs" .= wrapExceptions txOutputs,
    "txInputSummaries" .= txInputSummaries,
    "txSSigners" .= txSSigners ]

-- | If an exception is found then we tag the value as an exception.
-- By forcing evaluation of exceptions we prevent uncaught exceptions being thrown
-- and crashing faeServer.
wrapExceptions :: forall a. (ToJSON a) => a -> Value
wrapExceptions val = 
  unsafePerformIO $ catchAll (evaluate $ force $ toJSON val)
    (return . object . pure . ("exception",) . A.String . T.pack . show)

-- | If parsing fails then we look for the tagged exception.
readJSONField :: forall a. (FromJSON a) => Text -> Object -> Parser a
readJSONField fieldName obj = 
  obj .: fieldName <|> (obj .: fieldName >>= exceptionValue) 

exceptionValue :: Object -> Parser a
exceptionValue x = throw . TXFieldException <$> x .: "exception"

instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    exceptionValue o <|>
      TXInputSummary
        <$> readJSONField "txInputNonce" o
        <*> readJSONField "txInputOutputs" o
        <*> readJSONField "txInputVersions" o
      
instance FromJSON TXSummary where
  parseJSON = withObject "TXSummary" $ \o -> do
    TXSummary
      <$> o .: "transactionID"
      <*> readJSONField "txResult" o
      <*> readJSONField "txOutputs" o
      <*> o .: "txInputSummaries"
      <*> o .: "txSSigners"

instance FromJSON PublicKey where
  parseJSON = withText "VersionID" $ \pKey -> do
    either fail return $ readEither (T.unpack pKey)

instance ToJSON PublicKey where
  toJSON = toJSON . T.pack . show

instance FromJSON VersionID where
  parseJSON = withText "VersionID" $ \vID -> do
    either fail return $ readEither (T.unpack vID)

instance ToJSON VersionID where
  toJSON = toJSON . T.pack . show

instance ToJSON ContractID where
  toJSON = toJSON . show

instance FromJSON ContractID where
  parseJSON = withText "ContractID" $ \cID -> do
    either fail return $ readEither (T.unpack cID)

instance ToJSON Digest where
  toJSON = toJSON . show

instance FromJSON Digest where
  parseJSON = withText "Digest" $ \dig -> do
    either fail return $ readEither (T.unpack dig)

instance ToJSON UnquotedString where
  toJSON = toJSON . show

instance FromJSON UnquotedString where
  parseJSON = fmap UnquotedString . parseJSON
    
encodeJSON :: (ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a

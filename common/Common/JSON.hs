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

import qualified Data.ByteString.Char8 as C8

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=))

import Data.Aeson (eitherDecode, FromJSON, ToJSON, Object, toJSON, parseJSON, object, Value(..), withText, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Text (Text)
import Data.Typeable

import System.IO.Unsafe

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Text.Read

-- handle any exception in any  field as a exception for txinputsummary
instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = unsafePerformIO $ 
    catchAll 
      (do 
        evalTXInputNonce <- evaluate $ force $ txInputNonce
        evalTXInputOutputs <- evaluate $ force $ txInputOutputs
        evalTXInputVersions <- evaluate $ force $ txInputVersions
        return $ object [
          "txInputNonce" .= evalTXInputNonce,
          "txInputOutputs" .= evalTXInputOutputs,
          "txInputVersions" .= evalTXInputVersions ])
      $ return . object . pure . (T.pack "exception",) . A.String . T.pack . show

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= (writeJSONField txResult),
    "txOutputs" .= (writeJSONField txOutputs),
    "txInputSummaries" .= txInputSummaries,
    "signers" .= signers ]

-- | If an exception is found then we tag the value as an exception.
-- By forcing evaluation of exceptions we prevent uncaught exceptions being thrown
-- and crashing faeServer.
writeJSONField :: forall a. (ToJSON a) => a -> Value
writeJSONField val = 
  unsafePerformIO $ catchAll (evaluate $ force $ toJSON val)
    (return . object . pure . ("exception",) . A.String . T.pack . show)

-- | If parsing fails then we look for the tagged exception.
readJSONField :: forall a. (FromJSON a) => Text -> Object -> Parser a
readJSONField fieldName obj = 
  obj .: fieldName <|> do 
    x <- obj .: fieldName
    (throw . TXFieldException) <$> x .: "exception"

instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    TXInputSummary
      <$> readWithFailure o "txInputNonce"
      <*> readWithFailure o "txInputOutputs"
      <*> readWithFailure o "txInputVersions"
    where readWithFailure obj fieldName = obj .: fieldName 
            <|> (throw . TXFieldException) <$> obj .: "exception"
      
instance FromJSON TXSummary where
  parseJSON = withObject "TXSummary" $ \o -> do
    TXSummary
      <$> o .: "transactionID"
      <*> readJSONField "txResult" o
      <*> readJSONField "txOutputs" o
      <*> o .: "txInputSummaries"
      <*> o .: "signers"

instance FromJSON PublicKey where
  parseJSON = withText "VersionID" $ \pKey -> do
    either fail return $ readEither (T.unpack pKey)

instance ToJSON PublicKey where
  toJSON vID = toJSON $ T.pack $ show vID

instance ToJSON ShortContractID where
  toJSON scid = toJSON $ T.pack $ show scid

instance FromJSON VersionID where
  parseJSON = withText "VersionID" $ \vID -> do
    either fail return $ readEither (T.unpack vID)

instance ToJSON VersionID where
  toJSON vID = toJSON $ T.pack $ show vID

instance Read TypeRep where
  readsPrec = readsPrec

instance FromJSON TypeRep where
  parseJSON (A.String a) = either fail return $ readEither (T.unpack a)

instance ToJSON TypeRep where
  toJSON a = toJSON $ T.pack $ show a

instance FromJSON ShortContractID where
  parseJSON = withText "ShortContractID" $ \scid -> do
    either fail return $ readEither (T.unpack scid)
    
encodeJSON :: (ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a

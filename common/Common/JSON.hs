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

import Control.DeepSeq

import Data.Aeson (eitherDecode, FromJSON, ToJSON, Object, toJSON, parseJSON, object, Value(..), withText, withObject, (.=), (.:), (.:?))
import qualified Data.Aeson as A
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Maybe
import Data.Text (Text)
import Data.Typeable

import System.IO.Unsafe

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Text.Read

--- | Flushes out all exceptions present in the input, returning a formatted
--- error message if one is found.
--forceShow :: a -> m a
forceEval a = 
  unsafePerformIO $ catchAll (evaluate $ force a) (return . showException)
    
-- | Actually prints the exception nicely.  Due to call stack cruft we only
-- take the first line.
showException :: SomeException -> String
showException e = "<exception> " ++ (safeHead $ lines $ show e) where
  safeHead [] = []
  safeHead (x : _) = x

instance ToJSON TXInputSummary where
  toJSON TXInputSummary{..} = object [
    "txInputTXID" .= show txInputTXID,
    "txInputNonce" .= show txInputNonce,
    "txInputOutputs" .= writeJSONField (A.String $ T.pack $ show txInputOutputs),
    "txInputVersions" .= writeJSONField (A.String $ T.pack $ show txInputVersions) ]

instance ToJSON TXSummary where
  toJSON TXSummary{..} = object [
    "transactionID" .= show transactionID,
    "txResult" .= writeJSONField (A.String $ T.pack txResult),
    "txOutputs" .= writeJSONField (A.String $ T.pack $ show txOutputs),
    "txInputSummaries" .= txInputSummaries,
    "signers" .= signers ]

-- | If an exception is found then we tag the value as an exception.
-- By forcing evaluation of exceptions we prevent uncaught exceptions being thrown
-- and crashing faeServer.
writeJSONField :: Value -> Value
writeJSONField val = 
  unsafePerformIO $ catchAll (evaluate $ force val)
    (return . object . pure . ("exception",) . A.String . T.pack . showException)

-- | If parsing fails then we look for the tagged exception.
readJSONField :: forall a. (FromJSON a) => Text -> Object -> Parser a
readJSONField fieldName obj = do
  valM <- obj .:? fieldName
  case valM of
    Just val -> return val
    Nothing -> do
      message <- obj .: "exception"
      return $ throw $ userError message
  
instance FromJSON TXInputSummary where
  parseJSON = withObject "TXInputSummary" $ \o -> do
    TXInputSummary
      <$> o .: "txInputTXID"
      <*> readWithFailure "txInputNonce" o
      <*> readWithFailure "txInputOutputs" o
      <*> readWithFailure "txInputVersions" o
    where readWithFailure fieldName o = either fail return . readEither =<< readJSONField fieldName o

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
    txOutputs' <- readJSONField "txOutputs" o
    txInputSummaries <- o .: "txInputSummaries"
    signers <- o .: "signers"
    let txOutputs = fromMaybe raiseErr $ readMaybe $ fromMaybe raiseErr txOutputs'
    return TXSummary{..}
    where raiseErr = error "Can't parse txOutputs when decoding TXSummary JSON"
    
encodeJSON ::(ToJSON a) => a -> String
encodeJSON a = T.unpack $ X.toStrict $ D.decodeUtf8 $ A.encode a

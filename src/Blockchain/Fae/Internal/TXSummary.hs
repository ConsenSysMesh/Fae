{- |
Module: Blockchain.Fae.Internal.TXSummary
Description: Displaying Fae storage
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module defines TXSummary type aswell as the Pretty Fae Class and associated pretty-printing instances.
Note that the pretty-printing has to be done in a 'FaeStorage' monad, so the class can't just be 'Pretty'.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.TXSummary (
    collectTransaction,
    TXSummary(..),
    TXInputSummary(..),
    TransactionID
) where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions
import Blockchain.Fae.Internal.TX

import Common.Lens hiding ((.=))

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.Text.Lazy.Encoding as T

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Data.List
import Data.Typeable
import Data.Void

import GHC.Generics hiding (to)

import Text.PrettyPrint.Annotated
import Text.PrettyPrint.Annotated.HughesPJClass

-- | Outputs decorated with what they were output from.  Sort of
-- a proto-ContractID.
data OutputOf = 
  OutputOfTransaction TransactionID Outputs |
  OutputOfContract TransactionID ShortContractID Outputs

-- | Single input entry decorated with the nonce and transaction.
data InputOf = InputOf TransactionID ShortContractID Int InputOutputVersions
-- | Inputs map decorated with call order and transaction.
data InputsOf = InputsOf TransactionID [ShortContractID] InputOutputs
-- | Transaction entry decorated with transaction.
data EntryOf = EntryOf TransactionID TransactionEntry

-- | Useful for Fae clients communicating with faeServer
data TXSummary = TXSummary {
  transactionID :: ShortContractID,
  txResult :: String,
  txOutputs:: [(Int, ShortContractID)],
  txInputSummaries :: [TXInputSummary],
  signers :: [(String, PublicKey)]
} deriving (Show, Generic)

data TXInputSummary = TXInputSummary {
  txInputTXID :: ShortContractID,
  txInputNonce :: Int,
  txInputOutputs :: [(Int, ShortContractID)],
  txInputVersions :: [(VersionID, TypeRep)]
} deriving (Show, Generic)

makeLenses ''TXSummary

-- | Helpful shorthand; we don't use annotations.
type VDoc = Doc Void

instance Pretty ShortContractID where 
  pPrint scid = text (show scid)

instance Pretty TXSummary where
   pPrint TXSummary{..} = prettyHeader header entry
    where header = labelHeader "Transaction" transactionID
          result = prettyPair ("result", UnquotedString txResult)
          inputs = vcat $ pPrint <$> txInputSummaries
          outputs = prettyPair ("outputs", (pPrint txOutputs))
          signers' = prettyList "signers" signers 
          entry = vcat [ result, outputs, signers', inputs ]

instance Pretty TXInputSummary where
  pPrint TXInputSummary{..} = prettyHeader header inputBody
    where header = labelHeader "input" txInputTXID
          outputs = prettyPair ("outputs", txInputOutputs)
          versions = prettyPair ("versions", txInputVersions)
          nonce = prettyPair ("nonce", txInputNonce)
          inputBody = vcat [ nonce, outputs, versions ]

-- | Constructs a header with a name and some other data.
labelHeader :: (Show a) => String -> a -> Doc ann
labelHeader h l = text h <+> text (show l)

-- | Formats a nice header with indented body.
prettyHeader :: Doc ann -> Doc ann -> Doc ann
prettyHeader header body = header <> colon $+$ nest 2 body 

-- | Converts a string and list of "lines" into a body with header.
prettyList :: (Show v) => String -> [(String, v)] -> Doc ann
prettyList headString bodyList = 
  prettyHeader (text headString) (prettyPairs bodyList)

-- | Converts a list of pairs into a display, without header.
prettyPairs :: (Show v) => [(String, v)] -> Doc ann
prettyPairs = vcat . map prettyPair

-- | Prints a key-value pair with a colon.
prettyPair :: (Show v) => (String, v) -> Doc ann
prettyPair (x, y) = text x <> colon <+> text (show y)

-- | Get a JSON string which can be decoded to TXSummary for the convenience of faeServer clients
collectTransaction :: (MonadState Storage m, MonadCatch m, MonadIO m) => TransactionID -> m TXSummary
collectTransaction txID = do
  TransactionEntry{..} <- use $ _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  let 
      transactionID = txID
      signers = Map.toList $ getSigners txSigners
      txInputSCIDs = nub inputOrder
      txResult = show result 
      txOutputs = getTXOutputs (OutputOfTransaction txID outputs)
  txInputSummaries <- getInputSummary txID  txInputSCIDs inputOutputs
  return $ TXSummary{..}

getInputSummary :: (MonadState Storage m, MonadCatch m, MonadIO m) => TransactionID -> [ShortContractID] -> Map.Map ShortContractID InputOutputVersions -> m [TXInputSummary]
getInputSummary txID inputSCIDs inputMap = do
    forM (nub inputSCIDs) $ \scID -> do
      let 
        input = Map.findWithDefault (throw $ BadInputID txID scID) scID inputMap
        ~InputOutputVersions{..} = input
        txInputVersions = Map.toList $ getVersionMap iVersions
        txInputOutputs = getTXOutputs (OutputOfContract txID scID iOutputs)
      txInputNonce <- use $ nonceAt iRealID . to (fmap snd) . defaultLens (-1)
      return TXInputSummary {txInputTXID = txID, ..}

getTXOutputs :: OutputOf -> [(Int, ShortContractID)]
getTXOutputs outs = outputsToList $ outputCIDs outs
  where
    outputsToList = IntMap.toList . IntMap.map shorten
    outputCIDs (OutputOfTransaction txID outputs) = 
      makeOutputCIDs (TransactionOutput txID) outputs
    outputCIDs (OutputOfContract txID scID outputs) =
      makeOutputCIDs (InputOutput txID scID) outputs
    makeOutputCIDs makeCID Outputs{..} = 
      IntMap.mapWithKey (\i _ -> makeCID i) outputMap

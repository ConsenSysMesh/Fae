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
    displayException,
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

import System.IO.Unsafe

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Data.List

import GHC.Generics hiding (to)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- | Outputs decorated with what they were output from. Sort of
-- a proto-ContractID.
data OutputOf = 
  OutputOfTransaction TransactionID Outputs |
  OutputOfContract TransactionID ShortContractID Outputs

-- | Useful for Fae clients communicating with faeServer
data TXSummary = TXSummary {
  transactionID :: ShortContractID,
  txResult :: String,
  txOutputs:: [(Int, ShortContractID)],
  txInputSummaries :: [(ShortContractID, TXInputSummary)],
  signers :: [(String, PublicKey)]
} deriving (Show, Generic)

-- | Summarizes a single input contract call.
data TXInputSummary = TXInputSummary {
  txInputNonce :: Int,
  txInputOutputs :: [(Int, ShortContractID)],
  txInputVersions :: [(VersionID, String)]
} deriving (Show, Generic)

makeLenses ''TXSummary

-- | Helpful shorthand; we don't use annotations.
type VDoc = Doc

instance Pretty ShortContractID where 
  pPrint scid = text $ show scid

instance Pretty TXSummary where
   pPrint TXSummary{..} = prettyHeader header entry
    where header = labelHeader "Transaction" transactionID
          result = prettyPair ("result", displayException $ text txResult)
          outputs = displayException $ prettyList "outputs" $ (_1 %~ show) <$> txOutputs
          signers' = prettyList "signers" signers
          inputs = vcat $ (\(scid, txInputSummary) -> 
            prettyHeader (labelHeader "input" scid) (displayException $ pPrint txInputSummary)) 
              <$> txInputSummaries
          entry = vcat [ result, outputs, signers', inputs ]

instance Pretty TXInputSummary where
  pPrint TXInputSummary{..} = vcat [ nonce, outputs, versions ] 
    where outputs = prettyList "outputs" $ (_1 %~ show) <$> txInputOutputs
          versions = prettyHeader (text "versions" <> colon) $ prettyPairs $
            bimap show UnquotedString <$> txInputVersions
          nonce = prettyPair ("nonce", text $ show txInputNonce)

-- | Constructs a header with a name and some other data.
labelHeader :: (Show a) => String -> a -> Doc
labelHeader h l = text h <+> text (show l)

-- | Formats a nice header with indented body.
prettyHeader :: Doc -> Doc -> Doc
prettyHeader header body = header $+$ nest 2 body 

-- | Converts a string and list of "lines" into a body with header.
prettyList :: (Show v) => String -> [(String, v)] -> Doc
prettyList headString bodyList = 
  prettyHeader (text headString <> colon) (displayException $ prettyPairs bodyList)

-- | Converts a list of pairs into a display, without header.
prettyPairs :: (Show v) => [(String, v)] -> Doc
prettyPairs = vcat . map prettyPair

-- | Prints a key-value pair with a colon.
prettyPair :: (Show v) => (String, v) -> Doc
prettyPair (x, y) = text x <> colon <+> (text $ show y)

-- | Actually prints the exception nicely.  Due to call stack cruft we only
-- take the first line.
showException :: SomeException -> VDoc
showException e = text "<exception>" <+> text (safeHead $ lines $ show $ UnquotedString $ show e) where
  safeHead [] = []
  safeHead (x : _) = x

--- | Flushes out all exceptions present in the input, returning a formatted
--- error message if one is found.
displayException :: VDoc -> VDoc
displayException doc = 
  unsafePerformIO $ catchAll (evaluate $ force doc) (return . showException) 

-- | Get a JSON string which can be decoded to TXSummary for the convenience of faeServer clients
collectTransaction :: (MonadState Storage m, MonadCatch m, MonadIO m) => TransactionID -> m TXSummary
collectTransaction txID = do
  TransactionEntry{..} <- use $ _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  let 
    transactionID = txID
    signers = Map.toList $ getSigners txSigners
    txInputSCIDs = nub inputOrder
    txResult = show result 
    txOutputs = getTXOutputs $ OutputOfTransaction txID outputs
  txInputSummaries <- getInputSummary txID txInputSCIDs inputOutputs
  return $ TXSummary{..}

-- | Get the TXInputSummary for a gicen ShortContractID 
getInputSummary :: 
  (MonadState Storage m, MonadCatch m, MonadIO m) =>
  TransactionID -> 
  [ShortContractID] -> 
  Map.Map ShortContractID InputResults -> 
  m [(TransactionID, TXInputSummary)]
getInputSummary txID inputSCIDs inputMap = do
    forM (nub inputSCIDs) $ \scID -> do
      let 
        input = Map.findWithDefault (throw $ BadInputID txID scID) scID inputMap
        ~InputResults{..} = input
        txInputVersions = 
          over (traverse . _2) show $ Map.toList $ getVersionMap iVersions
        txInputOutputs = 
          maybe 
            (throw $ ContractOmitted txID scID) 
            (getTXOutputs . OutputOfContract txID scID) 
            iOutputsM
      txInputNonce <- 
        use $ nonceAt (withoutNonce iRealID) . to (fmap snd) . defaultLens (-1)
      return (scID, TXInputSummary {..})

-- | Gets the index of each output within faeStorage and the associated ShortContractID 
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

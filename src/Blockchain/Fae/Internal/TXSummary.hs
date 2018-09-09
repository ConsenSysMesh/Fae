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

import Blockchain.Fae.Internal.Contract
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
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Data.Foldable
import Data.List
import Data.Typeable

import GHC.Generics hiding (to)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- | Useful for Fae clients communicating with faeServer
data TXSummary = TXSummary {
  transactionID :: ShortContractID,
  txResult :: String,
  txOutputs:: Vector UnquotedString,
  txInputSummaries :: [(ShortContractID, TXInputSummary)],
  txSSigners :: [(String, PublicKey)]
} deriving (Show, Generic)

data TXInputSummary = TXInputSummary {
  txInputNonce :: Int,
  txInputOutputs :: Vector UnquotedString,
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
          outputs = displayException $ 
            prettyList "outputs" $ (_1 %~ show) <$> toIxList txOutputs
          txSSigners' = prettyList "signers" txSSigners
          inputs = vcat $ (\(scid, txInputSummary) -> 
            prettyHeader (labelHeader "input" scid) (displayException $ pPrint txInputSummary)) 
              <$> txInputSummaries
          entry = vcat [ result, outputs, txSSigners', inputs ]

instance Pretty TXInputSummary where
  pPrint TXInputSummary{..} = vcat [ nonce, outputs, versions ] where 
    outputs = prettyList "outputs" $ (_1 %~ show) <$> toIxList txInputOutputs
    versions = prettyHeader (text "versions" <> colon) $ prettyPairs $
      bimap show UnquotedString <$> txInputVersions
    nonce = prettyPair ("nonce", text $ show txInputNonce)

toIxList :: Vector a -> [(Int, a)]
toIxList = zip [0 ..] . toList

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

-- | Flushes out all exceptions present in the input, returning a formatted
-- error message if one is found.
displayException :: VDoc -> VDoc
displayException doc = 
  unsafePerformIO $ catchAll (evaluate $ force doc) (return . showException) 

-- | Get a JSON string which can be decoded to TXSummary for the
-- convenience of faeServer clients
collectTransaction :: (MonadState Storage m, MonadCatch m, MonadIO m) => TransactionID -> m TXSummary
collectTransaction txID = do
  TransactionEntry{..} <- 
    use $ _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  let transactionID = txID
      txSSigners = Map.toList $ getSigners txSigners
      txResult = show result 
      txOutputs = showTypes outputs
      txInputSummaries = toList $ getInputSummary txID inputResults
  return $ TXSummary{..}

-- | Get the TXInputSummary for a given ShortContractID 
getInputSummary :: 
  TransactionID -> 
  Vector InputResults -> 
  Vector (ShortContractID, TXInputSummary)
getInputSummary txID = Vector.imap $ \ix ~InputResults{..} -> 
  let txInputVersions = 
        over (traverse . _2) show $ Map.toList $ getVersionMap iVersions
      txInputOutputs = 
        maybe 
          (throw $ ContractOmitted txID ix) 
          showTypes 
          iOutputsM
      nonce = either (const Current) contractNonce iRealID
      txInputNonce
        | Nonce n <- nonce = n + 1
        | Current <- nonce = -1
  in (txID, TXInputSummary{..})

showTypes :: Outputs -> Vector UnquotedString
showTypes = fmap $ UnquotedString . show . outputType


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
module Blockchain.Fae.Internal.TXSummary where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.TX

import Common.Lens

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
  transactionID :: TransactionID,
  txResult :: String,
  txOutputs:: Vector VersionID,
  txInputSummaries :: Vector (ContractID, TXInputSummary),
  txSSigners :: [(String, PublicKey)]
} deriving (Generic)

data TXInputSummary = TXInputSummary {
  txInputStatus :: Status,
  txInputOutputs :: Vector VersionID,
  txInputVersion :: VersionID
} deriving (Generic)

instance Pretty ContractID where 
  pPrint = text . prettyContractID

instance Pretty TXSummary where
  pPrint TXSummary{..} = prettyHeader header (displayException entry) where 
    header = labelHeader "Transaction" transactionID
    result = prettyPair ("result", displayException $ text txResult)
    outputs = prettyVector "outputs" txOutputs
    txSSigners' = prettyList "signers" txSSigners
    inputs = txInputSummaries & vcat . toList . Vector.imap
      (\ix (cID, txInputSummary@TXInputSummary{..}) -> 
        let printCID = pPrint cID <+> parens (text $ show txInputStatus) 
            prettyInput = pPrint txInputSummary
        in prettyHeader 
             (prettyPair ("input #" ++ show ix, printCID))
             (displayException prettyInput))
    entry = vcat [ result, outputs, txSSigners', inputs ]

instance Pretty TXInputSummary where
  pPrint TXInputSummary{..} = vcat $ pVersion [outputs] where 
    pVersion
      | Updated <- txInputStatus = (prettyPair ("version", txInputVersion) :)
      | otherwise = id
    outputs = prettyVector "outputs" txInputOutputs

-- | Get a well-typed 'TXSummary' that can be communicated from the server
-- to a user (i.e. @faeServer@ to @postTX@) as JSON.
collectTransaction :: 
  (MonadState Storage m, MonadCatch m, MonadIO m) => TransactionID -> m TXSummary
collectTransaction txID = do
  TransactionEntry{..} <- 
    use $ _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  let transactionID = txID
      txSSigners = Map.toList $ getSigners txSigners
      txResult = show result 
      txOutputs = makeOut <$> outputs
      txInputSummaries = getInputSummary txID inputResults
  return $ TXSummary{..}

-- | Get the 'TXInputSummary' for a given 'TransactionID' 
getInputSummary :: 
  TransactionID -> Vector InputResults -> Vector (ContractID, TXInputSummary)
getInputSummary txID = Vector.imap $ \ix ~iR@InputResults{..} -> 
  let txInputVersion = iVersionID
      txInputOutputs = 
        maybe 
          (throw $ ContractOmitted txID ix) 
          (fmap $ makeOut) 
          iOutputsM
  in (iRealID, TXInputSummary{txInputStatus = iStatus, ..})

makeOut :: Output -> VersionID
-- We assume (justifiably!) that a new output does, in fact, store a live
-- contract.
makeOut Output{storedContract=Just StoredContract{..},..} = storedVersion

-- | Constructs a header with a name and some other data.
labelHeader :: (Show a) => String -> a -> Doc
labelHeader h l = text h <+> text (show l)

-- | Formats a nice header with indented body.
prettyHeader :: Doc -> Doc -> Doc
prettyHeader header body = header $+$ nest 2 body 

prettyVector :: (Show v) => String -> Vector v -> Doc
prettyVector headString v 
  | Vector.null v = empty
  | otherwise = prettyList headString $ Vector.toList $ Vector.imap ((,) . show) v

-- | Converts a string and list of "lines" into a body with header.
prettyList :: (Show v) => String -> [(String, v)] -> Doc
prettyList headString bodyList = 
  prettyHeader (text headString <> colon) (displayException $ prettyPairs bodyList)

-- | Converts a list of pairs into a display, without header.
prettyPairs :: (Show v) => [(String, v)] -> Doc
prettyPairs = vcat . map prettyPair

-- | Prints a key-value pair with a colon.
prettyPair :: (Show v) => (String, v) -> Doc
prettyPair (x, y) = text x <> colon <+> text (show y)

-- | Actually prints the exception nicely.  Due to call stack cruft we only
-- take the first line.
showException :: SomeException -> Doc
showException e = text "<exception>" <+> text (safeHead $ lines $ show $ UnquotedString $ show e) where
  safeHead [] = []
  safeHead (x : _) = x

-- | Flushes out all exceptions present in the input, returning a formatted
-- error message if one is found.
displayException :: Doc -> Doc
displayException doc = 
  unsafePerformIO $ catchAll (evaluate $ force doc) (return . showException) 


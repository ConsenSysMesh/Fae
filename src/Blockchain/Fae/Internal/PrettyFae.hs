{- |
Module: Blockchain.Fae.Internal.PrettyFae
Description: Displaying Fae storage
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

A class for pretty-printing Fae storage.  This has to be done in a 'FaeStorageT' monad, so the class can't just be 'Pretty'.
-}
module Blockchain.Fae.Internal.PrettyFae where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Data.List
import Data.Void

import Text.PrettyPrint.Annotated
import Text.PrettyPrint.Annotated.HughesPJClass

-- | Outputs decorated with what they were output from.  Sort of
-- a proto-ContractID.
data OutputOf c = 
  OutputOfTransaction TransactionID (OutputsT c) |
  OutputOfContract TransactionID ShortContractID (OutputsT c)

-- | Single input entry decorated with the nonce and transaction.
data InputOf c = InputOf TransactionID ShortContractID Int (InputOutputVersionsT c)
-- | Inputs map decorated with call order and transaction.
data InputsOf c = InputsOf TransactionID [ShortContractID] (InputOutputsT c)
-- | Transaction entry decorated with transaction.
data EntryOf c = EntryOf TransactionID (TransactionEntryT c)

-- | Helpful shorthand; we don't use annotations.
type VDoc = Doc Void

-- | This class isn't really necessary, as it's not used outside this
-- module, but it helps keep the number of function names down.
class PrettyFae a where
  prettyFae :: (MonadCatch m, MonadIO m) => a -> FaeStorageT m c VDoc

instance Pretty Signers where
  pPrint = prettyList "signers" . Map.toList . getSigners 

instance PrettyFae (OutputOf c) where
  prettyFae outs = return $
    prettyHeader (text "outputs") $
      prettyPairs $ outputsToList $ outputCIDs outs
    where
      outputsToList = map (_1 %~ show) . IntMap.toList . IntMap.map shorten
      outputCIDs (OutputOfTransaction txID outputs) = 
        makeOutputCIDs (TransactionOutput txID) outputs
      outputCIDs (OutputOfContract txID scID outputs) =
        makeOutputCIDs (InputOutput txID scID) outputs
      makeOutputCIDs makeCID OutputsT{..} = 
        IntMap.mapWithKey (\i _ -> makeCID i) outputMap

instance PrettyFae VersionRepMap where
  prettyFae vers = return $
    prettyHeader (text "versions") $
      prettyPairs $ map (_1 %~ show) $ Map.toList $ getVersionMap vers

instance PrettyFae (InputOf c) where
  prettyFae (InputOf txID scID n ~InputOutputVersions{..}) = do
    outputsD <- prettyFae (OutputOfContract txID scID iOutputs)
    versionsD <- prettyFae iVersions
    let nonceD = prettyPair ("nonce", n)
    inputBody <- displayException $ vcat
      [
        nonceD,
        outputsD,
        versionsD
      ]
    return $ prettyHeader header inputBody
    where header = labelHeader "input" scID

instance PrettyFae (InputsOf c) where
  prettyFae (InputsOf txID inputSCIDs ~(InputOutputs inputMap)) = 
    fmap vcat $ forM (nub inputSCIDs) $ \scID -> do
      let 
        input = Map.findWithDefault (throw $ BadInputID txID scID) scID inputMap
        ~InputOutputVersions{..} = input
      storage <- get
      let n = snd $ storage ^. nonceAt iRealID . defaultLens (undefined, -1)
      prettyFae $ InputOf txID scID n input

instance PrettyFae (EntryOf c) where
  prettyFae (EntryOf txID ~TransactionEntry{..}) = do
    resultD <- displayException $ text $ show result
    outputsD <- prettyFae $ OutputOfTransaction txID outputs
    inputsD <- prettyFae $ InputsOf txID inputOrder inputOutputs
    return $ prettyHeader header $ vcat
      [
        prettyPair ("result", resultD),
        outputsD,
        pPrint signers,
        inputsD
      ]
    where header = labelHeader "Transaction" txID

-- | Convenience function for neatly showing a 'TransactionEntry' by ID,
-- rather than actually going into the storage to get and format it.  It
-- catches all exceptions thrown by contracts or transactions and prints
-- an error message instead; thus, whatever actually did complete is part
-- of the output.
showTransaction ::
  (MonadCatch m, MonadIO m) => TransactionID -> FaeStorageT m c String
showTransaction txID = do
  entry <- use $ 
    _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  render <$> prettyFae (EntryOf txID entry)

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

-- | Flushes out all exceptions present in the input, returning a formatted
-- error message if one is found.
displayException :: 
  (MonadCatch m, MonadIO m) => VDoc -> m VDoc
displayException doc = 
  catchAll (liftIO $ evaluate $ force doc) (return . showException)

showException :: SomeException -> VDoc
showException e = text "<exception>" <+> text (show e)


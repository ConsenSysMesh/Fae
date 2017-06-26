module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae
import Blockchain.Fae.Contracts
import Blockchain.Fae.Internal
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad
import Control.Monad.Fix

import Data.Dynamic

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

runTransaction :: TransactionID -> PublicKey -> Fae () -> Fae ()
runTransaction txID sender x = handleAsync setOutputException $ do
  transient <- newTransient sender
  Fae $ _transientState .= transient
  () <- x -- Force evaluation to flush out exceptions
  -- If x throws an exception, we don't save anything
  saveFee
  saveEscrows
  saveTransient txID

  where
    setOutputException e = Fae $ do
      _persistentState . _outputs . _useOutputs . at txID ?= Left e

newTransient :: PublicKey -> Fae FaeTransient
newTransient senderKey = Fae $ do
  entries <- use $ _persistentState . _entries
  lastHash <- use $ _persistentState . _lastHash
  credit <- use $ _parameters . _transactionCredit
  return $
    FaeTransient
    {
      entryUpdates = entries,
      newOutput = Output Nothing Map.empty,
      escrows = Escrows Map.empty,
      sender = senderKey,
      lastHashUpdate = lastHash,
      currentEntry = Nothing,
      currentFacet = zeroFacet,
      currentFee = credit,
      currentFeeLeft = credit,
      localLabel = Seq.empty
    }

saveFee :: Fae ()
saveFee = do
  currentFee <- Fae $ use $ _transientState . _currentFee
  _ <- escrow FeeToken getFee currentFee
  return ()

saveEscrows :: Fae ()
saveEscrows = Fae $ do
  escrows <- use $ _transientState . _escrows . _useEscrows
  getFae $ label "escrows" $ sequence_ $ Map.mapWithKey convertEscrow escrows

convertEscrow :: EntryID -> Escrow -> Fae ()
convertEscrow entryID escrow = do
  key <- signer
  _ <- label (Text.pack $ show entryID) $ mfix $ addEntry . makeEntry key
  return ()

  where
    makeEntry key newEntryID = Entry fDyn (toDyn c) (toDyn a) where
      fDyn = contractMaker escrow newEntryID key
      c = const @Signature @Signature
      a = undefined :: Signature

saveTransient :: TransactionID -> Fae ()
saveTransient txID = Fae $ do
  entryUpdates <- use $ _transientState . _entryUpdates
  _persistentState . _entries .= entryUpdates
  newOutput <- use $ _transientState . _newOutput
  _persistentState . _outputs . _useOutputs . at txID ?= Right newOutput
  lastHashUpdate <- use $ _transientState . _lastHashUpdate
  _persistentState . _lastHash .= lastHashUpdate


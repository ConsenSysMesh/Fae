module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae
import Blockchain.Fae.Contracts
import Blockchain.Fae.Internal
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

runTransaction :: TransactionID -> PublicKey -> Fae () -> Fae ()
runTransaction txID sender x = do
  transient <- newTransient sender
  Fae $ _transientState .= transient
  x
  -- If x throws an exception, we don't save anything
  saveFee
  saveEscrows
  saveTransient txID

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
      currentEntry = nullEntry,
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
  getFae $ label "escrows" $
    forM_ (Map.keys escrows) $ \k ->
      label (Text.pack $ show k) $ output k
  facet <- use $ _transientState . _currentFacet
  entries <- getFae $ sequence $ Map.mapWithKey (convertEscrow facet) escrows
  _persistentState . _entries . _useEntries %= Map.union entries

convertEscrow :: FacetID -> EntryID -> Escrow -> Fae Entry
convertEscrow = undefined

saveTransient :: TransactionID -> Fae ()
saveTransient txID = Fae $ do
  entryUpdates <- use $ _transientState . _entryUpdates
  _persistentState . _entries .= entryUpdates
  newOutput <- use $ _transientState . _newOutput
  _persistentState . _outputs . _useOutputs . at txID ?= newOutput
  lastHashUpdate <- use $ _transientState . _lastHashUpdate
  _persistentState . _lastHash .= lastHashUpdate


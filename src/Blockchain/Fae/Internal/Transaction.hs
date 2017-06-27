module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae
import Blockchain.Fae.Internal
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix

import Data.Dynamic
import Data.IORef

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

runTransaction :: TransactionID -> PublicKey -> Fae () -> Fae ()
runTransaction txID sender x = handleAsync setOutputException $ do
  transient <- newTransient txID sender
  Fae $ _transientState .= transient
  () <- x -- Force evaluation to flush out exceptions
  -- If x throws an exception, we don't save anything
  saveEscrows
  saveTransient

newTransient :: TransactionID -> PublicKey -> Fae FaeTransient
newTransient txID@(TransactionID txID0) senderKey = Fae $ do
  pStateRef <- use _persistentState
  pState <- liftIO $ readIORef pStateRef
  let
    entries = pState ^. _entries
    lastHash = pState ^. _lastHash
  cFacet <- use _currentFacet
  liftIO $ writeIORef cFacet zeroFacet
  return $
    FaeTransient
    {
      entryUpdates = entries,
      newOutput = Output Nothing Map.empty,
      escrows = Escrows Map.empty,
      sender = senderKey,
      lastHashUpdate = lastHash <> txID0,
      currentTransaction = txID,
      currentEntry = Nothing,
      localLabel = Seq.empty
    }

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

setOutputException :: SomeException -> Fae ()
setOutputException e = Fae $ do
  txID <- use $ _transientState . _currentTransaction
  facetIDRef <- use _currentFacet
  facetID <- liftIO $ readIORef facetIDRef
  pState <- use _persistentState
  liftIO $ modifyIORef' pState $
    _outputs . _useOutputs . at txID . each . _facetException ?~
      ExceptionPlus facetID e


module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import qualified Data.Map as Map
import Data.Sequence (Seq)

import Data.Dynamic

newtype Transaction a = Transaction { getTransaction :: Contract () () a }

newTransaction ::
  (Typeable valType) =>
  [InputContract] ->
  (Seq Dynamic -> Fae ([OutputContract], valType)) ->
  Transaction valType
newTransaction inputCs f = Transaction $
  newPureContract inputCs $ \retVals _ -> f retVals

runTransaction :: 
  (Typeable a) =>
  TransactionID -> PublicKey -> Transaction a -> Fae ()
runTransaction txID sender x = 
  handleAll (setException txID) $ do
    transient <- newTransient txID sender
    Fae $ _transientState .= transient
    retVal <- evalContract Nothing (getTransaction x) ()
    -- If x throws an exception, we don't save anything
    saveTransient
    saveReturnValue txID retVal

newTransient :: TransactionID -> PublicKey -> Fae FaeTransient
newTransient (TransactionID txID) senderKey = Fae $ do
  contracts <- use _contracts
  return $
    FaeTransient
    {
      contractUpdates = contracts,
      escrowUpdates = Escrows Map.empty,
      sender = senderKey,
      lastHash = txID
    }

saveTransient :: Fae ()
saveTransient = Fae $ do
  contracts <- use $ _transientState . _contractUpdates
  _contracts .= contracts
  escrows <- use $ _transientState . _escrowUpdates
  _escrows .= escrows

setException :: TransactionID -> SomeException -> Fae ()
setException txID e = Fae $
  _transactions . _useTransactions . at txID ?= Left e

saveReturnValue :: (Typeable a) => TransactionID -> a -> Fae ()
saveReturnValue txID x = Fae $
  _transactions . _useTransactions . at txID ?= Right (toDyn x)
  

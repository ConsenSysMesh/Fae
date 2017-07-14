module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Fae
import Blockchain.Fae.Internal.Lens

import Control.Monad.Reader
import Control.Monad.Trans

import qualified Data.Map as Map
import Data.Sequence (Seq)

import Data.Dynamic

newtype Transaction a = Transaction { getTransaction :: Contract () () a }
type FaeTransaction a = FaeContract () () a

newTransaction ::
  (Typeable valType) =>
  CallTree ->
  FaeTransaction valType ->
  Transaction valType
newTransaction callTree faeTransaction = Transaction $
  newContract callTree () faeTransaction

runTransaction :: 
  (Typeable a) =>
  TransactionID -> PublicKey -> Transaction a -> Fae ()
runTransaction txID@(TransactionID d) sender x = 
  handleAll (setException txID) $ do
    transient <- newTransient sender
    Fae $ _transientState .= transient
    retVal <- evalContract [] d (ContractID d) (getTransaction x) ()
    -- If x throws an exception, we don't save anything
    saveContracts
    saveReturnValue txID retVal

newTransient :: PublicKey -> Fae FaeTransient
newTransient senderKey = Fae $ do
  contracts <- use _contracts
  return $
    FaeTransient
    {
      contractUpdates = contracts,
      contractEscrows = Escrows Map.empty,
      sender = senderKey
    }

saveContracts :: Fae ()
saveContracts = Fae $ do
  contracts <- use $ _transientState . _contractUpdates
  _contracts .= contracts

setException :: TransactionID -> SomeException -> Fae ()
setException txID e = Fae $
  _transactions . _useTransactions . at txID ?= Left e

saveReturnValue :: (Typeable a) => TransactionID -> a -> Fae ()
saveReturnValue txID x = Fae $ do
  _contracts . _useContracts . at (toContractID txID) .= Nothing
  _transactions . _useTransactions . at txID ?= Right (toDyn x)
  
toContractID :: TransactionID -> ContractID
toContractID (TransactionID d) = ContractID d

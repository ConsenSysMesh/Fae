{- |
Module: Blockchain.Fae.Internal.Exceptions
Description: Wrapper library for "Control.Monad.Catch"
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module just re-exports "Control.Monad.Catch", as well as 'Typeable' so that we can derive 'Exception' with just this module imported, and also 'throw' and 'evaluate' from "Control.Exception", which seem not to be re-exported elsewhere.
-}
module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch,
    Ex.throw, Ex.evaluate,
    T.Typeable
  ) where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.IDs.Types
import qualified Control.Exception as Ex
import Control.Monad.Catch hiding (displayException)
import Data.ByteString (ByteString)
import Data.Typeable as T

import System.IO.Unsafe

-- * Functions

-- | Obviously not pure at all, but intended to be used only in extremely
-- limited circumstances; namely, to decide what to do with the global
-- update of a contract function or nonce, or escrow function, in the event
-- of an exception.
unsafeIsDefined :: a -> Bool
unsafeIsDefined act = unsafePerformIO $ catchAll 
  (Ex.evaluate act >> return True) 
  (const $ return False)

-- * Types

-- | Exceptions for version-related errors.
data VersionException =
  BadVersionID Int VersionID TypeRep |
  BadVersionedType VersionID TypeRep TypeRep |
  UnresolvedVersionID VersionID |
  UnexpectedResolvedVersion

-- | Exceptions for storage-related errors.
data StorageException =
  BadTransactionID TransactionID |
  BadContractID ContractID |
  BadInputID TransactionID Int |
  BadNonce ContractID Int Int |
  InvalidNonceAt ContractID |
  ContractOmitted TransactionID Int |
  CantImport ByteString TypeRep |
  ImportWithoutNonce ContractID |
  DeletedEntry 

-- | Exceptions for contract-related errors.
data ContractException =
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID |
  BadEscrowName EntryID TypeRep TypeRep |
  MissingSigner String |
  NotStartState EntryID VersionID

-- | Exceptions for transaction-related errors.
data TransactionException =
  BadInputVersion (Maybe VersionID) VersionID |
  IncompleteContract ContractID |
  IncompleteTransaction TransactionID |
  NotEnoughInputs |
  TooManyInputs

newtype TXFieldException = TXFieldException String

-- * Instances

-- | - 
instance Show TXFieldException where
  show (TXFieldException e) = e

-- | -
instance Show VersionException where
  show (BadVersionID ix vID rep) = 
    "No version found in input contract #" ++ show ix ++ 
    " with ID: " ++ show vID ++
    " (expected type: " ++ show rep ++ ")"
  show (BadVersionedType vID bad good) = 
    "For value with version ID: " ++ show vID ++ 
    "; expected type: " ++ show good ++ 
    "; got: " ++ show bad
  show (UnresolvedVersionID vID) = "Unresolved version ID: " ++ show vID
  show UnexpectedResolvedVersion = 
    "Found a resolved version where version ID was expected."

-- | -
instance Show StorageException where
  show (BadTransactionID tID) = "Not a transaction ID: " ++ show tID
  show (BadContractID cID) = "Not a contract ID: " ++ prettyContractID cID
  show (BadInputID txID ix) = 
    "No input contract with index " ++ show ix ++ 
    " for transaction " ++ show txID
  show (BadNonce cID bad good) = 
    "Contract " ++ prettyContractID cID ++ 
    " has nonce " ++ show good ++ "; got: " ++ show bad
  show (InvalidNonceAt cID) = "Can't look up contract ID: " ++ prettyContractID cID
  show (ContractOmitted txID ix) =
    "Contract call #" ++ show ix ++ 
    " in transaction " ++ show txID ++ 
    " was replaced with an imported return value."
  show (CantImport bs ty) =
    "Can't decode value of type " ++ show ty ++ " from bytes: " ++ printHex bs
  show (ImportWithoutNonce cID) =
    "Rejecting imported value for " ++ prettyContractID cID ++ 
    " that lacks a nonce value."
  show (DeletedEntry) =
    "(internal error) Tried to delete an entry of the transaction results!"

-- | -
instance Show ContractException where
  show (BadInputParse input inputType) = 
    "Unable to parse '" ++ show input ++ "' as type: " ++ show inputType
  show (BadArgType bad good) = 
    "Expected argument type: " ++ show good ++ "; got: " ++ show bad
  show (BadValType bad good) =
    "Expected value type: " ++ show good ++ "; got: " ++ show bad
  show (BadEscrowID eID) = "No escrow found in this contract with ID: " ++ show eID
  show (BadEscrowName entID bad good) =
    "Wrong contract name for escrow " ++ show entID ++ 
    "; got " ++ show bad ++ "; expected " ++ show good
  show (MissingSigner name) = "No signer named " ++ show name
  show (NotStartState entID vID) = 
    "Escrow " ++ show entID ++ 
    " with version " ++ show vID ++ 
    " is not in its starting state"

-- | -
instance Show TransactionException where
  show (BadInputVersion badM good) =
    "Expected input value version: " ++ show good ++ "; got: " ++
    maybe "none" show badM
  show (IncompleteContract cID) =
    "Contract " ++ show cID ++ " has missing result"
  show (IncompleteTransaction txID) =
    "Transaction " ++ show txID ++ " has missing result"
  show NotEnoughInputs = "Transaction expected more inputs"
  show TooManyInputs = "Transaction expected fewer inputs"

-- | -
instance Exception VersionException
-- | -
instance Exception StorageException
-- | -
instance Exception ContractException
-- | -
instance Exception TransactionException
-- | -
instance Exception TXFieldException

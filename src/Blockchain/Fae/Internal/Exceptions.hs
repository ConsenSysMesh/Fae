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

import Blockchain.Fae.Internal.IDs.Types
import qualified Control.Exception as Ex
import Control.Monad.Catch
import Data.Typeable as T

-- * Types
--
-- | Exceptions for ID-related errors.
-- There may be more exceptions in the future; shouldn't be a newtype
data IDException =
  InvalidContractID ContractID

-- | Exceptions for version-related errors.
data VersionException =
  BadVersionID ShortContractID VersionID |
  BadVersionedType VersionID TypeRep TypeRep |
  UnresolvedVersionID VersionID |
  UnexpectedResolvedVersion

-- | Exceptions for storage-related errors.
data StorageException =
  BadTransactionID TransactionID |
  BadInputID ShortContractID |
  BadNonce ContractID Int Int |
  InvalidNonceAt ContractID

-- | Exceptions for contract-related errors.
data ContractException =
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID |
  MissingSigner String

-- | Exceptions for transaction-related errors.
data TransactionException =
  NotEnoughInputs |
  TooManyInputs |
  BadInput ContractID

-- * Instances

-- | -
instance Show IDException where
  show (InvalidContractID cID) = "Invalid contract ID: " ++ show cID

-- | -
instance Show VersionException where
  show (BadVersionID scID vID) = 
    "No version found in contract " ++ show scID ++ " with ID: " ++ show vID
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
  show (BadInputID sID) = "No input contract with short ID: " ++ show sID
  show (BadNonce cID bad good) = 
    "Contract " ++ show cID ++ " has nonce " ++ show good ++ "; got: " ++ show bad
  show (InvalidNonceAt cID) = "Can't look up contract ID: " ++ show cID

-- | -
instance Show ContractException where
  show (BadInputParse input inputType) = 
    "Unable to parse '" ++ show input ++ "' as type: " ++ show inputType
  show (BadArgType bad good) = 
    "Expected argument type: " ++ show good ++ "; got: " ++ show bad
  show (BadValType bad good) =
    "Expected value type: " ++ show good ++ "; got: " ++ show bad
  show (BadEscrowID eID) = "No escrow found in this contract with ID: " ++ show eID
  show (MissingSigner name) = "No signer named " ++ show name

-- | -
instance Show TransactionException where
  show NotEnoughInputs = "Transaction expected more inputs"
  show TooManyInputs = "Transaction expected fewer inputs"
  show (BadInput cID) = "No input contract with ID: " ++ show cID

-- | -
instance Exception IDException
-- | -
instance Exception VersionException
-- | -
instance Exception StorageException
-- | -
instance Exception ContractException
-- | -
instance Exception TransactionException


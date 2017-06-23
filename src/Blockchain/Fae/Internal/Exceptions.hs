module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Exception.Safe
  ) where

import Blockchain.Fae.Internal.Types

import Control.Exception.Safe

import Control.Monad.IO.Class

import Data.Typeable

data EntryException =
  BadEntryID EntryID |
  WrongFacet EntryID FacetID FacetID |
  BadEntryArgType EntryID TypeRep TypeRep |
  BadEntryValType EntryID TypeRep TypeRep
  deriving (Typeable, Show)

data EscrowException =
  BadEscrowID EntryID |
  BadPublicType EntryID TypeRep TypeRep |
  BadPrivateType EntryID TypeRep TypeRep 
  deriving (Typeable, Show)

data FacetException =
  NotAFacet FacetID |
  InsufficientFee FacetID Fee Fee |
  NotADependentFacet FacetID FacetID
  deriving (Typeable, Show)

data TransactionException =
  BadTransactionID TransactionID
  deriving (Typeable, Show)

instance Exception EntryException 
instance Exception EscrowException
instance Exception FacetException
instance Exception TransactionException

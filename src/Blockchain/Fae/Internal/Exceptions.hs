module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Ex0
  ) where

import Blockchain.Fae.Internal.Types

import Control.Exception as Ex0 hiding (throwIO)
import qualified Control.Exception as Ex (throwIO)

import Control.Monad.IO.Class

import Data.Typeable

throwIO :: (Exception e, MonadIO m) => e -> m a
throwIO = liftIO . throwIO

data EntryException =
  BadEntryID EntryID |
  WrongFacet EntryID FacetID FacetID |
  BadEntryArgType EntryID TypeRep TypeRep |
  BadEntryValType EntryID TypeRep TypeRep
  deriving (Typeable, Show)

data EscrowException =
  BadEscrowID EscrowID |
  BadPublicType EscrowID TypeRep TypeRep |
  BadPrivateType EscrowID TypeRep TypeRep 
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

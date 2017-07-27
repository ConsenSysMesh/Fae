module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch,
    Ex.throw
  ) where

import Blockchain.Fae.Internal.Monads

import qualified Control.Exception as Ex

import Control.Monad.Catch

import Control.Monad.IO.Class

import Data.Typeable

data TransactionException =
  BadContractID ContractID |
  BadTransactionID TransactionID |
  BadInputID ShortContractID |
  BadInput ContractID 
  deriving (Show, Typeable)

data ContractException =
  OpenEscrows ContractID |
  MissingInput Int |
  BadArgType ContractID TypeRep TypeRep | 
  BadValType ContractID TypeRep TypeRep
  deriving (Show, Typeable)

data EscrowException =
  BadEscrowType EntryID TypeRep TypeRep |
  BadEscrowID EntryID |
  StatefulEscrow ContractID
  deriving (Typeable, Show)

instance Exception TransactionException 
instance Exception ContractException 
instance Exception EscrowException

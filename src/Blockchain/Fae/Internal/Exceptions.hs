module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch,
    Ex.throw
  ) where

import Blockchain.Fae.Internal.IDs

import qualified Control.Exception as Ex

import Control.Monad.Catch

import Data.Typeable

data FaeException =
  BadTransactionID TransactionID |
  BadContractID ContractID |
  BadInputID ShortContractID |
  BadInput ContractID |
  NotEnoughInputs |
  TooManyInputs |
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID |
  OpenEscrows 
  deriving (Typeable, Show)

instance Exception FaeException 

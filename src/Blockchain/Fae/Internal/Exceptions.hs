module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch,
    Ex.throw
  ) where

import Blockchain.Fae.Internal.IDs

import qualified Control.Exception as Ex

import Control.Monad.Catch

import Control.Monad.IO.Class

import Data.Typeable

data FaeException =
  BadTransactionID TransactionID |
  BadContractID ContractID |
  BadInputID ShortContractID |
  BadInput ContractID |
  BadChainedInput ContractID Int |
  UntrustedInput ContractID ContractID |
  MissingInput Int |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID |
  OpenEscrows 
  deriving (Typeable, Show)

instance Exception FaeException 

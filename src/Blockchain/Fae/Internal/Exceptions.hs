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

data ContractException =
  BadContractID ContractID |
  BadContract ContractID SomeException |
  MissingOutput ContractID |
  MissingInput Int |
  BadAncestor ContractID Int |
  BadArgType ContractID TypeRep TypeRep | 
  BadValType ContractID TypeRep TypeRep
  deriving (Show, Typeable)

data EscrowException =
  BadEscrowID EntryID |
  BadEscrowType EntryID TypeRep TypeRep
  deriving (Typeable, Show)

instance Exception ContractException 
instance Exception EscrowException

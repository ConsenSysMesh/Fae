module Blockchain.Fae.Internal.Exceptions
  (
    module Blockchain.Fae.Internal.Exceptions,
    module Control.Monad.Catch
  ) where

import Blockchain.Fae.Internal.Types

import Control.Monad.Catch

import Control.Monad.IO.Class

import Data.Typeable

data ContractException =
  BadContractID ContractID |
  BadContract ContractID SomeException |
  BadArgType ContractID TypeRep TypeRep | 
  BadValType ContractID TypeRep TypeRep
  deriving (Show, Typeable)

data EscrowException =
  BadEscrowID EntryID |
  BadEscrowType EntryID TypeRep TypeRep
  deriving (Typeable, Show)

instance Exception ContractException 
instance Exception EscrowException

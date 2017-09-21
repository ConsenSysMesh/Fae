module Blockchain.Fae.Internal.IDs where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto

import Data.Foldable

import GHC.Generics

{- Types -}

data ContractID =
  JustTransaction TransactionID |
  TransactionOutput TransactionID Int |
  InputOutput TransactionID ShortContractID Int
  deriving (Show, Generic)

newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Show, Serialize)

type TransactionID = ShortContractID -- For simplicity

type EntryID = (TransactionID, Int)
data EscrowID argType valType = EscrowID EntryID
data AnEscrowID = forall argType valType. AnEscrowID (EscrowID argType valType)

data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

{- Typeclasses -}

class HasEscrowIDs a where
  getEscrowIDs :: a -> [AnEscrowID]

{- Instances -}

instance Serialize ContractID
instance Digestible ContractID
instance Digestible ShortContractID

instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  getEscrowIDs _ = []

instance HasEscrowIDs (EscrowID argType valType) where
  getEscrowIDs = (:[]) . anEscrowID

instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b) where
  getEscrowIDs (x, y) = getEscrowIDs x ++ getEscrowIDs y

instance {-# OVERLAPPABLE #-} 
  (HasEscrowIDs a, Foldable t) => HasEscrowIDs (t a) where

  getEscrowIDs = concatMap getEscrowIDs . toList

{- Functions -}

shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

anEscrowID :: EscrowID argType valType -> AnEscrowID
anEscrowID eID = AnEscrowID eID

bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue


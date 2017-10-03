module Blockchain.Fae.Internal.IDs where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto

import Data.Foldable
import Data.Void

import GHC.Generics

import Numeric.Natural

{- Types -}

-- | This identifier locates a contract in storage.  It is not intended to
-- be used in contract code, as indeed, a contract can never be called
-- explicitly but only as a transaction input, for which there is a special
-- syntax outside Haskell.
data ContractID =
  JustTransaction TransactionID |
  TransactionOutput TransactionID Int |
  InputOutput TransactionID ShortContractID Int
  deriving (Show, Generic)

-- | The hash of a 'ContractID', useful for abbreviating what would
-- otherwise be unboundedly long chains of contracts that are outputs of
-- contracts that are outputs of ... that are outputs of some long-ago
-- transaction.
newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Show, Serialize)

type TransactionID = ShortContractID -- For simplicity

type EntryID = (TransactionID, Int)
-- | This identifier locates an escrow.  Escrow IDs are assigned when the
-- escrow is first created and are guaranteed to be globally unique and
-- immutable.  Each escrow ID is valid only within a contract that actually
-- holds the escrow, and the type parameters must correspond to the
-- escrow's actual argument and value types.  Escrow IDs may therefore only
-- be constructed by the 'newEscrow' function.  However, they should appear
-- type-correct in contract signatures to formally verify that the contract
-- receives and returns a particular kind of opaque value, e.g. a currency.
data EscrowID argType valType = EscrowID EntryID
-- | An existential type unifying all escrow IDs.  Since it cannot be
-- passed to 'useEscrow', it cannot be used to subvert escrow value typing.
-- Instead, it is used to allow heterogeneous escrow IDs in the return type
-- of 'getEscrowIDs'.
data AnEscrowID = forall argType valType. AnEscrowID (EscrowID argType valType)
-- | A wrapper that does /not/ officially contain an escrow ID.  Thus,
-- a transactional escrow ID does not allow its escrow to be transferred to
-- new contracts or escrows; it must be used in the place it was returned.
newtype TXEscrowID argType valType = TXEscrowID (EscrowID argType valType)

-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.
data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

{- Typeclasses -}

-- | Every contract must accept arguments and return values in this class.
-- The returned list /must/ contain, in any order, the IDs of every escrow
-- upon which the type 'a' depends for its value.  These escrows will be
-- transferred along with a value of type 'a' whenever it is returned from
-- a contract.  One usually need not define this class explicitly, as
-- suitable general instances are given below.
class HasEscrowIDs a where
  getEscrowIDs :: a -> [AnEscrowID]
  default getEscrowIDs :: (Generic a, GHasEscrowIDs (Rep a)) => a -> [AnEscrowID]
  getEscrowIDs = gGetEscrowIDs . from

class GHasEscrowIDs f where
  gGetEscrowIDs :: f p -> [AnEscrowID]

{- Instances -}

instance Serialize ContractID
instance Digestible ContractID
instance Digestible ShortContractID

instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  getEscrowIDs _ = []

instance {-# OVERLAPPABLE #-} 
  (Foldable f, HasEscrowIDs a) => HasEscrowIDs (f a) where

  getEscrowIDs = concatMap getEscrowIDs . toList

instance (HasEscrowIDs a) => HasEscrowIDs (Maybe a)
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (Either a b)
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b)

instance 
  (HasEscrowIDs argType, HasEscrowIDs valType) => 
  HasEscrowIDs (EscrowID argType valType) where

  getEscrowIDs eID = [AnEscrowID eID]

-- | This instance enforces the default that a type contains no escrows to
-- a law for transactional escrows.  
instance HasEscrowIDs (TXEscrowID argType valType) where
  getEscrowIDs _ = []

instance HasEscrowIDs Void where
  getEscrowIDs _ = []

-- Boring Generic boilerplate

instance GHasEscrowIDs V1 where
  gGetEscrowIDs _ = undefined

instance GHasEscrowIDs U1 where
  gGetEscrowIDs U1 = []

instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :+: g) where
  gGetEscrowIDs (L1 x) = gGetEscrowIDs x
  gGetEscrowIDs (R1 x) = gGetEscrowIDs x

instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :*: g) where
  gGetEscrowIDs (x :*: y) = gGetEscrowIDs x ++ gGetEscrowIDs y

-- Recurse into sub-structures
instance {-# OVERLAPPABLE #-} (HasEscrowIDs c) => GHasEscrowIDs (K1 i c) where
  gGetEscrowIDs (K1 x) = getEscrowIDs x

instance (GHasEscrowIDs f) => GHasEscrowIDs (M1 i t f) where
  gGetEscrowIDs (M1 x) = gGetEscrowIDs x

{- Functions -}

-- | Take the hash of a contract ID.
shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

-- | Generalize an escrow ID.
anEscrowID :: EscrowID argType valType -> AnEscrowID
anEscrowID eID = AnEscrowID eID

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue


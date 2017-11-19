{- |
Module: Blockchain.Fae.Internal.IDs
Description: Identifier types and associated functions
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

There are several identifier types in Fae: two kinds of contract ID (one detailed, one convenient but lossy), transaction IDs, and escrow IDs.  In addition, the escrow IDs in particular have a lot of ritual surrounding them.
-}
{-# LANGUAGE DataKinds #-}
module Blockchain.Fae.Internal.IDs where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens hiding (from, to)

import Control.DeepSeq

import Control.Monad.Writer

import Data.Char
import Data.Functor.Identity
import qualified Data.Serialize as Ser
import Data.List
import Data.String
import Data.Traversable
import Data.Typeable
import Data.Void

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

import Text.ParserCombinators.ReadP

-- * Types

-- | This identifier locates a contract in storage.  It is not intended to
-- be used in contract code, as indeed, a contract can never be called
-- explicitly but only as a transaction input, for which there is a special
-- syntax outside Haskell.
data ContractID =
  -- | Technically, a transaction is a type of contract, but this kind of
  -- contract ID is basically only for errors.
  JustTransaction TransactionID |
  -- | One way contracts can be created is by being directly output by
  -- a transaction.  The outputs are indexed from 0 in order of creation.
  TransactionOutput TransactionID Int |
  -- | The other way contracts can be created is by being output during the
  -- execution of one of its input contract calls.  The outputs are indexed
  -- from 0 in order of creation, with the indexing specific to each input
  -- contract.
  InputOutput TransactionID ShortContractID Int
  deriving (Read, Show, Generic)

-- | The hash of a 'ContractID', useful for abbreviating what would
-- otherwise be unboundedly long chains of contracts that are outputs of
-- contracts that are outputs of ... that are outputs of some long-ago
-- transaction.
newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Serialize, IsString, NFData)

-- | For simplicity
type TransactionID = ShortContractID
-- | For simplicity
type BlockID = Digest

-- | For simplicity
type EntryID = Digest

-- | This identifier locates an escrow.  Escrow IDs are assigned when the
-- escrow is first created and are guaranteed to be globally unique and
-- immutable.  Each escrow ID is valid only within a contract or other
-- escrow that actually holds the escrow, and the type parameters must
-- correspond to the escrow's actual argument and value types.  Escrow IDs
-- may be constructed by the 'newEscrow' function or from string literals.
-- However, they should appear type-correct in contract signatures to
-- formally verify that the contract receives and returns a particular kind
-- of opaque value, e.g. a currency.
newtype EscrowID argType valType = EscrowID { entID :: EntryID }
  deriving (Generic)
-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.
data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

-- | Exceptions for ID-related errors.
data IDException =
  BadInputEscrow String |
  NotEscrowOut EntryID
  deriving (Typeable, Show)

-- | A map of escrow IDs that preserves input and output types, regardless
-- of what they are.
type EscrowIDMap f =
  forall argType valType. 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  EscrowID argType valType -> f (EscrowID argType valType)

-- | The type of a traversal by an 'EscrowIDMap', used in 'HasEscrowIDs'.
-- Note that because the kind of traversal map that is allowed is subject
-- to some Fae-specific constraints, this is a little different from
-- a @lens@ 'Traversal'.
type EscrowIDTraversal a = 
  forall f. (Monad f) => EscrowIDMap f -> a -> f a

-- * Typeclasses

-- | Every contract must accept arguments and return values in this class.
-- The returned traversal /must/ contain, in any order, the IDs of every
-- escrow upon which the type 'a' depends for its value.  These escrows
-- will be transferred along with a value of type 'a' whenever it is
-- returned from a contract.  
--
-- Although this class must be instantiated for any user-defined types used
-- in contracts, we do not export the class members, so that only the
-- default instance may be used.
class HasEscrowIDs a where
  -- | Like 'traverse' from 'Traversable', except that it only covers the
  -- escrow IDs, which may be of heterogeneous types.
  traverseEscrowIDs :: EscrowIDTraversal a
  default 
    traverseEscrowIDs :: 
      (Generic a, GHasEscrowIDs (Rep a)) => 
      EscrowIDTraversal a
  traverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

-- | Generic backend for 'HasEscrowIDs'
class GHasEscrowIDs f where
  gTraverseEscrowIDs :: EscrowIDTraversal (f p)

-- * Instances

-- | Of course
instance Exception IDException

-- | Just so we can get 'Digestible'
instance Serialize ContractID
-- | So we can get a 'ShortContractID' from a regular one.
instance Digestible ContractID
-- | For 'faeServer'
instance NFData ContractID

-- | 'ShortContractID's and, by extension, 'TransactionIDs', are read as
-- the digests they wrap.
instance Read ShortContractID where
  readsPrec _ = fmap (_1 %~ ShortContractID) . readsPrec 0

-- | 'ShortContractID's and, by extension, 'TransactionIDs', show as hex
-- strings.  This should be inverse to the 'Read' instance.
instance Show ShortContractID where
  show (ShortContractID dig) = show dig

-- | So we can have escrow IDs as contract arguments.
instance Read (EscrowID argType valType) where
  readsPrec n = map (_1 %~ EscrowID) . readsPrec n 

-- | Escrow IDs, of course, contain themselves.  A tricky special case is
-- that the transactional variants contain escrows in their argument or
-- value as well.
instance 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  HasEscrowIDs (EscrowID argType valType) where

  -- Not point-free; we need to specialize the forall.
  traverseEscrowIDs f eID = f eID

-- | Default instance.
instance HasEscrowIDs Void 
-- | Default instance
instance HasEscrowIDs () 
-- | Default instance
instance HasEscrowIDs Bool
-- | Default instance
instance HasEscrowIDs Char where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | Default instance
instance HasEscrowIDs Int where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | Default instance
instance HasEscrowIDs Integer where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | Default instance
instance HasEscrowIDs Float where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | Default instance
instance HasEscrowIDs Double where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | Default instance
instance HasEscrowIDs Natural where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | This is just natural, though it can probably be covered in most
-- practical cases by the 'Generic' instance, if probably slower.
instance {-# OVERLAPPABLE #-} 
  (Traversable f, HasEscrowIDs a) => HasEscrowIDs (f a) where

  traverseEscrowIDs g = traverse (traverseEscrowIDs g)
-- | Default instance.
instance (HasEscrowIDs a) => HasEscrowIDs (Maybe a)
-- | Default instance.
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b)
-- | Default instance.
instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (Either a b)

-- Boring Generic boilerplate

-- | Empty types have no escrow IDs to apply the traversal function to.
instance GHasEscrowIDs V1 where
  gTraverseEscrowIDs _ = pure

-- | Constructors with no records have no escrow IDs to traverse.
instance GHasEscrowIDs U1 where
  gTraverseEscrowIDs _ = pure

-- | In a sum type, you traverse each alternative.
instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :+: g) where
  gTraverseEscrowIDs h = \case
    L1 x -> L1 <$> gTraverseEscrowIDs h x
    R1 x -> R1 <$> gTraverseEscrowIDs h x

-- | In a product type, you traverse both halves.
instance (GHasEscrowIDs f, GHasEscrowIDs g) => GHasEscrowIDs (f :*: g) where
  gTraverseEscrowIDs h (x :*: y) = 
    liftM2 (:*:) (gTraverseEscrowIDs h x) (gTraverseEscrowIDs h y)

-- | Recurse into nested types.
instance (HasEscrowIDs c) => GHasEscrowIDs (K1 i c) where
  gTraverseEscrowIDs f (K1 x) = K1 <$> traverseEscrowIDs f x

-- | We just ignore the metadata.
instance (GHasEscrowIDs f) => GHasEscrowIDs (M1 i m f) where
  gTraverseEscrowIDs g (M1 x) = M1 <$> gTraverseEscrowIDs g x 

-- * Functions

-- | Take the hash of a contract ID.
shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue

-- | For making empty instances of 'HasEscrowIDs'
defaultTraverseEscrowIDs :: EscrowIDTraversal a
defaultTraverseEscrowIDs _ x = return x -- Not point-free to specialize forall


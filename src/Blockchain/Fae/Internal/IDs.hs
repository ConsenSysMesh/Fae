{-# LANGUAGE DataKinds, GADTs #-}
{- |
Module: Blockchain.Fae.Internal.IDs
Description: Functions on identifier types
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module re-exports "Blockchain.Fae.Internal.IDs.Types" and provides some instances and functions for constructing and manipulating its types.
-}
module Blockchain.Fae.Internal.IDs 
  (
    module Blockchain.Fae.Internal.IDs.Types,
    module Blockchain.Fae.Internal.IDs
  ) where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs.Types

import Control.DeepSeq

import Control.Monad.Writer

import Data.Char
import Data.List
import Data.String
import Data.Traversable
import Data.Type.Equality
import Data.Void

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

import Type.Reflection

-- * Types

-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.  This is a restricted form of 'Dynamic' bearing
-- a 'HasEscrowIDs' constraint.
data BearsValue = forall a. (HasEscrowIDs a, Typeable a) => BearsValue a

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
-- We do not export the class members, so that only the default (automatic,
-- undecidable) instance may be used.
class HasEscrowIDs a where
  -- | Like 'traverse' from 'Traversable', except that it only covers the
  -- escrow IDs, which may be of heterogeneous types.
  traverseEscrowIDs :: EscrowIDTraversal a

-- | Generic backend for 'HasEscrowIDs'
class GHasEscrowIDs f where
  gTraverseEscrowIDs :: EscrowIDTraversal (f p)

{- Instances -}

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

-- | -
instance HasEscrowIDs Char where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Word where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Int where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Integer where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Float where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Double where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs Natural where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | -
instance HasEscrowIDs PublicKey where
  traverseEscrowIDs = defaultTraverseEscrowIDs

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
shorten = ShortContractID . digest . withoutNonce

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a, Typeable a) => a -> BearsValue
bearer = BearsValue 

-- | Like 'fromDynamic'.
unBearer :: forall a. (HasEscrowIDs a, Typeable a) => BearsValue -> Maybe a
unBearer (BearsValue x)
  | Just HRefl <- typeOf x `eqTypeRep` (typeRep :: TypeRep a) = Just x
  | otherwise = Nothing

-- | Like 'fromDyn'.
unBear :: forall a. (HasEscrowIDs a, Typeable a) => BearsValue -> a -> a
unBear (BearsValue x) x0 
  | Just HRefl <- typeOf x `eqTypeRep` typeOf x0 = x
  | otherwise = x0

-- | Like 'dynTypeRep'.
bearerType :: BearsValue -> SomeTypeRep
bearerType (BearsValue x) = someTypeRep (Just x)

-- | For making empty instances of 'HasEscrowIDs'
defaultTraverseEscrowIDs :: EscrowIDTraversal a
-- Not point-free to specialize forall
defaultTraverseEscrowIDs _ x = return x

-- | Predicate for determining whether a required nonce was provided.
hasNonce :: ContractID -> Bool
hasNonce (_ :# n) = True
hasNonce _ = False

-- | Normalizes a contract ID for storing with transaction inputs 
withoutNonce :: ContractID -> ContractID
withoutNonce (cID :# n) = cID
withoutNonce cID = cID


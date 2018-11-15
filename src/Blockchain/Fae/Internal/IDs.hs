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

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs.Types

import Common.Lens

import Control.Monad.State.Class
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Typeable

import GHC.Generics
import GHC.TypeLits

import Numeric.Natural

-- * Types

-- | An existential type unifying the 'HasEscrowIDs' class.  A value of
-- this type is, abstractly, something within a contract that has economic
-- value, in the sense that it is backed by a scarce resource contained in
-- an escrow.  This is a restricted form of 'Dynamic' bearing
-- a 'HasEscrowIDs' constraint.
data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

-- | A map of escrow IDs that preserves input and output types, regardless
-- of what they are.
type EscrowIDMap f = 
  forall name. (Typeable name) => EscrowID name -> f (EscrowID name)

-- | The type of a traversal by an 'EscrowIDMap', used in 'HasEscrowIDs'.
-- Note that because the kind of traversal map that is allowed is subject
-- to some Fae-specific constraints, this is a little different from
-- a @lens@ 'Traversal'.
type EscrowIDTraversal a = 
  forall m. (Monad m) => EscrowIDMap m -> a -> m a

-- * Typeclasses

-- | Every contract must accept arguments and return values in this class.
-- The returned traversal /must/ contain, in any order, the IDs of every
-- escrow upon which the type 'a' depends for its value.  These escrows
-- will be transferred along with a value of type 'a' whenever it is
-- returned from a contract.  
--
-- We do not export the class members, so that only the default (automatic,
-- undecidable) instance may be used.  'Typeable' is a constraint because
-- 'BearsValue' requires it.
class (Typeable a) => HasEscrowIDs a where
  -- | Like 'traverse' from 'Traversable', except that it only covers the
  -- escrow IDs, which may be of heterogeneous types.
  traverseEscrowIDs :: EscrowIDTraversal a

-- | Generic backend for 'HasEscrowIDs'
class GHasEscrowIDs f where
  gTraverseEscrowIDs :: EscrowIDTraversal (f p)

{- Instances -}

-- | Even though 'BearsValue' hides the true type, it still has
-- a 'HasEscrowIDs' constraint, so we can traverse the anonymous contents.
instance HasEscrowIDs BearsValue where
  traverseEscrowIDs f (BearsValue x) = BearsValue <$> traverseEscrowIDs f x

-- | Escrow IDs, of course, contain themselves.
instance (Typeable name) => HasEscrowIDs (EscrowID name) where
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
-- | This is important because a value may have function types in it, and
-- still hold escrows.  Functions are not 'Generic', so we have to bypass
-- the generic instance here.
instance (Typeable a, Typeable b) => HasEscrowIDs (a -> b) where
  traverseEscrowIDs = defaultTraverseEscrowIDs
-- | This covers a wide variety of types like 'Map' that are not 'Generic'
-- but still function as containers that might have escrow IDs.
instance 
  (Typeable (t a), HasEscrowIDs a, Traversable t) => 
  HasEscrowIDs (Container (t a)) where

  traverseEscrowIDs f = 
    fmap Container . traverse (traverseEscrowIDs f) . getContainer

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

-- | Mark a value backed by escrows as such.
bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer = BearsValue 

-- | Like 'fromDynamic'.
unBearer :: forall a. (Typeable a) => BearsValue -> Maybe a
unBearer (BearsValue x) = cast x

-- | Like 'fromDyn'.
unBear :: (Typeable a) => BearsValue -> a -> a
unBear (BearsValue x) x0 = fromMaybe x0 $ cast x

-- | Like 'dynTypeRep'.
bearerType :: BearsValue -> TypeRep
bearerType (BearsValue x) = typeRep (Just x)

-- | For making empty instances of 'HasEscrowIDs'
defaultTraverseEscrowIDs :: EscrowIDTraversal a
-- Not point-free to specialize forall
defaultTraverseEscrowIDs _ x = return x

-- | Just accumulates all the entries in each of the objects as a map.
-- Internally, this uses an imitation of the @lens@ function 'toList' for
-- 'Traversal's, but since an 'EscrowIDTraversal' is not /exactly/
-- a 'Traversal', we have to reproduce it.
getEntIDMap :: 
  forall a b m.
  (HasEscrowIDs b, Monad m) => 
  (forall name. (Typeable name) => EscrowID name -> m a) -> 
  b -> m (Map EntryID a)
getEntIDMap f = 
  fmap Map.fromList . sequence . execWriter . traverseEscrowIDs tellEntry 
  where 
    tellEntry :: 
      (Typeable name') => 
      EscrowID name' -> Writer [m (EntryID, a)] (EscrowID name')
    tellEntry eID@EscrowID{..} = tell [(entID,) <$> f eID] >> return eID

-- | Finds an entry or throws an error.
getEntry :: (MonadState (Map EntryID a) m) => EntryID -> m a
getEntry entID = fromMaybe (throw idErr) <$> use (at entID) where
  idErr = BadEscrowID entID

-- | This function actually /takes/ the entries, not just copies them,
-- because valuable things can't be copied.
takeEntry :: (MonadState (Map EntryID a) m) => EntryID -> m a
takeEntry entID = do
  x <- getEntry entID
  at entID .= Nothing
  return x

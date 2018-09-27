{-# LANGUAGE DataKinds, UndecidableInstances #-}
{- |
Module: Blockchain.Fae.Internal.Versions
Description: Versioning for contract return values
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Support for versioning of contract return values, so that vetted transactions are reliably safe from malicious code.  Other bugs that may arise from changes in non-valuable quantities are not protected, as it is desirable to allow transactions to depend on the current state of Fae.
-}
module Blockchain.Fae.Internal.Versions where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs

import Control.Monad
import Control.Monad.State

import Data.Foldable

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe

import Data.Proxy
import Data.Typeable

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector (write)

import qualified GHC.Generics as Gen (from, to)
import GHC.Generics hiding (from, to)

import GHC.TypeLits

import Numeric.Natural

-- * Type classes

-- | Types that can be versioned, meaning a unique identifier is calculated
-- for it and all subobjects (stopping at 'Versioned' fields).  Has an
-- automatic, undecidable instance for any 'Generic' type.
class (HasEscrowIDs a, Typeable a) => Versionable a where
  -- | Calculates the version of its argument, using the lookup function to
  -- account for any escrows it may contain.
  version :: (EntryID -> VersionID) -> a -> VersionID

-- | The one-parameter analogue of 'Versionable', for 'Generic' instances.
-- The default instance is not to have a version.
class GVersionable f where
  -- | Like 'version'
  gVersion :: (EntryID -> VersionID) -> f p -> VersionID

-- | For traversing the records of a product type.
class GRecords f where
  -- | The 'VersionMap' is of all the records in self, not including self.
  gRecords :: (EntryID -> VersionID) -> f p -> State Int VersionID

{- Instances -}

-- | This key base case actually uses the function argument to 'version'
-- by applying it to the escrow ID.
instance (HasEscrowIDs name) => Versionable (EscrowID name) where
  version = (. entID)

-- | -
instance Versionable Char where
  version = defaultVersion
-- | -
instance Versionable Word where
  version = defaultVersion
-- | -
instance Versionable Int where
  version = defaultVersion
-- | -
instance Versionable Integer where
  version = defaultVersion
-- | -
instance Versionable Float where
  version = defaultVersion
-- | -
instance Versionable Double where
  version = defaultVersion
-- | -
instance Versionable Natural where
  version = defaultVersion
-- | -
instance Versionable PublicKey where
  version = defaultVersion

-- | No type has no version
instance GVersionable V1 where
  gVersion _ _ = undefined

-- | Enumeration type has no version
instance GVersionable U1 where
  gVersion _ _ = nullID

-- | Sum type version equals summand version
instance 
  (GVersionable f, GVersionable g) => 
  GVersionable (f :+: g) where

  gVersion f (L1 x) = mkVersionID (0 :: Int, gVersion f x)
  gVersion f (R1 y) = mkVersionID (1 :: Int, gVersion f y)

-- | Data type version is the hash of the module and type names with the
-- constructor version.  For newtypes, we act as though we have
-- a 'Versioned' and ignore the version map of the inner type, the
-- principle being that a newtype should have a completely fresh interface
-- attached to an existing data representation.
instance 
  (GVersionable f, KnownSymbol tSym, KnownSymbol mSym) => 
  GVersionable (M1 D (MetaData tSym mSym p nt) f) where

  gVersion f m@(M1 x) = mkVersionID (mName, tName, gVersion f x) where
    mName = symbolVal (Proxy @mSym)
    tName = symbolVal (Proxy @tSym)

-- | Constructor version is the hash of the constructor name with the
-- list of record version.
instance
  (GRecords f, KnownSymbol cSym) =>
  GVersionable (M1 C (MetaCons cSym x s) f) where

  gVersion f (M1 x) = mkVersionID (cName, ver) where 
    ver = evalState (gRecords f x) 0
    cName = symbolVal (Proxy @cSym)

-- | Empty type has no records
instance GRecords V1 where
  gRecords _ _ = return undefined

-- | Type with no fields has no records
instance GRecords U1 where
  gRecords _ U1 = return nullID

-- | We completely ignore record selector metadata because we generate the
-- record numbering ourselves.
instance (GRecords f) => GRecords (M1 S md f) where
  gRecords f (M1 x) = gRecords f x

-- | Product type records are the concatenation of the factor records.
instance (GRecords f, GRecords g) => GRecords (f :*: g) where
  gRecords f (x :*: y) = mkVersionID <$> liftM2 (,) (gRecords f x) (gRecords f y)

-- | Recursive type is a record in itself.
instance (Versionable c) => GRecords (K1 i c) where
  gRecords f (K1 x) = do
    n <- get
    modify (+ 1)
    return $ mkVersionID (n, version f x)

-- * Functions
-- | For default instances of 'Versionable'
defaultVersion :: 
  (Digestible a) => 
  (EntryID -> VersionID) -> a -> VersionID
defaultVersion = const mkVersionID


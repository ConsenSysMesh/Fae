{-# LANGUAGE DataKinds #-}
{- |
Module: Blockchain.Fae.Internal.Versions
Description: The core 'Contract' type that underlies Fae
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Support for versioning of contract return values, so that vetted transactions are reliably safe from malicious code.  Other bugs that may arise from changes in non-valuable quantities are not protected, as it is desirable to allow transactions to depend on the current state of Fae.
-}
module Blockchain.Fae.Internal.Versions where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens

import Control.Monad
import Control.Monad.State

import Data.Dynamic
import Data.Foldable

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe

import Data.Proxy
import Data.Typeable
import Data.Void

import qualified GHC.Generics as Gen (from, to)
import GHC.Generics hiding (from, to)

import GHC.TypeLits

import Numeric.Natural

-- * Types

-- | For convenience
type VersionID = Digest
-- | The inverse to the map of subobjects to their versions.
type VersionMap = Map VersionID Dynamic
-- | For marking a contract input to be passed by version ID, or a contract
-- output to be considered a single coherent unit.
data Versioned a = 
  Versioned { getVersioned :: a } |
  VersionedID VersionID 

-- | Errors that can arise when dealing with versions.
data VersionErrors =
  BadVersionID VersionID |
  BadVersionedType TypeRep TypeRep |
  UnresolvedVersionID VersionID |
  UnexpectedResolvedVersion
  deriving (Show, Typeable)

-- * Type classes

-- | Types that can be versioned, meaning a unique identifier is calculated
-- for it and all subobjects (stopping at 'Versioned' fields).
class (Typeable a) => Versionable a where
  -- | Returns the map of all versions, including self.
  versionMap :: (EntryID -> VersionID) -> a -> VersionMap
  versionMap f x =
    -- Unlike in @GRecords m (K1 i c)@, the inherent version is actually
    -- the one we want, since the top-level object is not a record.
    let (ver, vers) = versions f x in
    -- Same comment as in @GRecords m (K1 i c)@
    Map.insert ver (toDyn x) vers

  -- | Sort of the inverse of 'versionMap', this resolves all 'VersionedID'
  -- variants of 'Versioned' to 'Versioned' variants.
  mapVersions :: VersionMap -> a -> a
  default 
    mapVersions :: 
      (Generic a, GVersionable (Rep a)) => 
      VersionMap -> a -> a
  mapVersions vMap = Gen.to . gMapVersions vMap . Gen.from where

  -- | Returns the version and the map of all strictly sub-versions.
  versions :: (EntryID -> VersionID) -> a -> (VersionID, VersionMap)
  default 
    versions :: 
      (Generic a, GVersionable (Rep a)) => 
      (EntryID -> VersionID) -> a -> (VersionID, VersionMap)
  versions f = gVersions f . Gen.from

-- | The one-parameter analogue of 'Versionable', for 'Generic' instances.
-- The default instance is not to have a version.
class GVersionable f where
  -- | Like 'versions'
  gVersions :: (EntryID -> VersionID) -> f p -> (VersionID, VersionMap)
  -- | Like 'mapVersions'
  gMapVersions :: VersionMap -> f p -> (f p)

-- | For traversing the records of a product type.
class GRecords f where
  -- | The 'VersionMap' is of all the records in self, not including self.
  gRecords :: (EntryID -> VersionID) -> f p -> State Int VersionMap
  -- | Like 'mapVersions'
  gRMapVersions :: VersionMap -> f p -> (f p)

{- Instances -}

-- | Of course
instance Exception VersionErrors

-- | Read 'Versioned' values as their version, which is a 'Digest'.
-- Regardless of whether 'a' has a 'Read' instance, 'Versioned a' always
-- does, though the version may fail to be resolved.
instance Read (Versioned a) where
  readsPrec n = fmap (_1 %~ VersionedID) . readsPrec n

-- | This instance enforces 'Versioned's as being opaque, unified types.
-- Ideally this designation should be accompanied by actual data hiding.
instance (Versionable a) => Versionable (Versioned a) where
  versions f (Versioned x) = (ver, Map.empty) where
    (ver, _) = versions f x 

  versionMap f (Versioned x) = versionMap f x

  mapVersions _ Versioned{} = throw $ UnexpectedResolvedVersion
  mapVersions vMap (VersionedID ver) =
    Versioned $ 
    fromMaybe (throw $ BadVersionedType (dynTypeRep xDyn) (typeRep $ Proxy @a)) $ 
    fromDynamic xDyn
    where xDyn = fromMaybe (throw $ BadVersionID ver) $ Map.lookup ver vMap

-- | Pass through the 'Versioned' constructor
instance (HasEscrowIDs a) => HasEscrowIDs (Versioned a) where
  traverseEscrowIDs f (Versioned x) = Versioned <$> traverseEscrowIDs f x
  traverseEscrowIDs _ (VersionedID ver) = throw $ UnresolvedVersionID ver

-- | This key base case actually uses the function argument to 'versions'
-- by applying it to the escrow ID.
instance 
  (Typeable argType, Typeable valType) => 
  Versionable (EscrowID argType valType) where

  versions f eID@EscrowID{..} = (f entID, Map.empty) 
  mapVersions _ eID = eID

-- | /Not/ the generic instance, because that makes just a ton of useless
-- versions of all the tails of a string, and all the characters.  Instead,
-- we treat a list as though it were 'Versioned'.  We take 'Foldable' to be
-- the class designating all things that are "like recursively nested
-- constructors", e.g. cons-lists.
instance {-# OVERLAPPABLE #-} 
  (Versionable a, Functor t, Foldable t, Typeable t) => 
  Versionable (t a) where

  versions f x = (digest $ map (fst . versions f) $ toList x, Map.empty)
  mapVersions = fmap . mapVersions

-- | Generic instance
instance Versionable Void
-- | Generic instance
instance Versionable ()
-- | Generic instance
instance Versionable Bool
-- | Default instance
instance Versionable Char where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable Int where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable Integer where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable Float where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable Double where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable Natural where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Default instance
instance Versionable PublicKey where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | Generic instance overlapping that for 'Foldable', since we actually do
-- want to go inside a 'Maybe'.
instance (Versionable a) => Versionable (Maybe a)
-- | Generic instance
instance (Versionable a, Versionable b) => Versionable (a, b)
-- | Generic instance
instance (Versionable a, Versionable b) => Versionable (Either a b)

-- | No type has no version
instance GVersionable V1 where
  gVersions _ _ = (undefined, Map.empty)
  gMapVersions _ _ = undefined

-- | Enumeration type has no version
instance GVersionable U1 where
  gVersions _ U1 = (digest (), Map.empty)
  gMapVersions _ U1 = U1

-- | Sum type version equals summand version
instance 
  (GVersionable f, GVersionable g) => 
  GVersionable (f :+: g) where

  gVersions f (L1 x) = gVersions f x
  gVersions f (R1 y) = gVersions f y

  gMapVersions vMap (L1 x) = L1 $ gMapVersions vMap x
  gMapVersions vMap (R1 x) = R1 $ gMapVersions vMap x

-- | Data type version is the hash of the module and type names with the
-- constructor version.
instance 
  (GVersionable f, KnownSymbol tSym, KnownSymbol mSym) => 
  GVersionable (M1 D (MetaData tSym mSym p nt) f) where

  gVersions f (M1 x) =
    let
      (ver, vers) = gVersions f x
      vID = digest (mName, tName, ver)
    -- Same comment as for @M1 C@
    in (vID, vers)

    where
      mName = symbolVal (Proxy @mSym)
      tName = symbolVal (Proxy @tSym)

  gMapVersions vMap (M1 x) = M1 $ gMapVersions vMap x

-- | Constructor version is the hash of the constructor name with the
-- list of record versions.
instance
  (GRecords f, KnownSymbol cSym) =>
  GVersionable (M1 C (MetaCons cSym x s) f) where

  gVersions f (M1 x) = 
    let 
      vers = evalState (gRecords f x) 0
      vID = digest (cName, Map.keys vers)
    -- 'x' is only a representation type, so we don't put it in the map.
    in (vID, vers)

    where cName = symbolVal (Proxy @cSym)

  gMapVersions vMap (M1 x) = M1 $ gRMapVersions vMap x

-- | Empty type has no records
instance GRecords V1 where
  gRecords _ _ = return Map.empty
  gRMapVersions _ _ = undefined

-- | Type with no fields has no records
instance GRecords U1 where
  gRecords _ U1 = return Map.empty
  gRMapVersions _ U1 = U1

-- | We completely ignore record selector metadata because we generate the
-- record numbering ourselves.
instance (GRecords f) => GRecords (M1 S md f) where
  gRecords f (M1 x) = gRecords f x
  gRMapVersions vMap (M1 x) = M1 $ gRMapVersions vMap x

-- | Product type records are the concatenation of the factor records.
instance (GRecords f, GRecords g) => GRecords (f :*: g) where
  gRecords f (x :*: y) = liftM2 Map.union (gRecords f x) (gRecords f y)
  gRMapVersions vMap (x :*: y) = gRMapVersions vMap x :*: gRMapVersions vMap y

-- | Recursive type is a record in itself.
instance (Versionable c) => GRecords (K1 i c) where
  gRecords f (K1 x) = do
    let (ver, vers) = versions f x
    n <- get
    modify (+ 1)
    -- The version as a record differs from the inherent version.  This
    -- is why we don't include self in 'versions' output.
    let vID = digest (n, ver)
    -- It's important that here, we have 'x' as its actual type, not as
    -- @Rep c@, should 'c' be 'Generic'.  Therefore its 'Dynamic' is
    -- meaningful.
    return $ Map.insert vID (toDyn x) vers

  gRMapVersions vMap (K1 x) = K1 $ mapVersions vMap x

-- * Functions
-- | For default instances of 'Versionable'
defaultVersions :: 
  (Digestible a) => 
  (EntryID -> VersionID) -> a -> (VersionID, VersionMap)
defaultVersions _ x = (digest x, Map.empty)

-- | For default instances of 'Versionable'
defaultMapVersions :: VersionMap -> a -> a
defaultMapVersions _ = id


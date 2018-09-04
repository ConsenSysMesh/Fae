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

import qualified GHC.Generics as Gen (from, to)
import GHC.Generics hiding (from, to)

import GHC.TypeLits

import Numeric.Natural

-- * Types

-- | Generalization of the two kinds of version maps.  The difference is
-- that one only retains type information.
newtype VersionMapT a = VersionMap { getVersionMap :: Map VersionID a }
-- | The inverse to the map of subobjects to their versions.
type VersionMap = VersionMapT BearsValue
-- | The one that only retains the types.
type VersionRepMap = VersionMapT TypeRep
-- | Versions by contract ID
newtype VersionMap' = VersionMap' (Map ShortContractID VersionMap)
-- | For marking a contract input to be passed by version ID, or a contract
-- output to be considered a single coherent unit.
data Versioned a = 
  Versioned { getVersioned :: a } |
  VersionedID ShortContractID VersionID 
  deriving (Generic)

-- | Purely to have a nice syntax for entering VersionedIDs.
data ReadVersioned = ShortContractID ::: VersionID deriving (Read)

-- * Type classes

-- | Types that can be versioned, meaning a unique identifier is calculated
-- for it and all subobjects (stopping at 'Versioned' fields).  Has an
-- automatic, undecidable instance for any 'Generic' type.
class (HasEscrowIDs a, Typeable a) => Versionable a where
  -- | Returns the map of all versions, including self.
  versionMap :: (EntryID -> VersionID) -> a -> VersionMap
  versionMap f x =
    -- Unlike in @GRecords m (K1 i c)@, the inherent version is actually
    -- the one we want, since the top-level object is not a record.
    let (ver, VersionMap vers) = versions f x in
    -- Same comment as in @GRecords m (K1 i c)@
    VersionMap $ Map.insert ver (bearer x) vers

  -- | Sort of the inverse of 'versionMap', this resolves all 'VersionedID'
  -- variants of 'Versioned' to 'Versioned' variants.
  mapVersions :: VersionMap' -> a -> a

  -- | Returns the version and the map of all strictly sub-versions.
  versions :: (EntryID -> VersionID) -> a -> (VersionID, VersionMap)

-- | The one-parameter analogue of 'Versionable', for 'Generic' instances.
-- The default instance is not to have a version.
class GVersionable f where
  -- | Like 'versions'
  gVersions :: (EntryID -> VersionID) -> f p -> (VersionID, VersionMap)
  -- | Like 'mapVersions'
  gMapVersions :: VersionMap' -> f p -> (f p)

-- | For traversing the records of a product type.
class GRecords f where
  -- | The 'VersionMap' is of all the records in self, not including self.
  gRecords :: (EntryID -> VersionID) -> f p -> State Int VersionMap
  -- | Like 'mapVersions'
  gRMapVersions :: VersionMap' -> f p -> (f p)

-- | For applying a conditional to a type-level bool
class IfK (b :: Bool) where
  ifK :: Proxy b -> a -> a -> a

{- Instances -}

-- | Like 'Maybe'.
instance Foldable Versioned where
  foldr f y0 (Versioned x) = f x y0
  foldr _ y0 _ = y0

-- | Like 'Maybe'.
instance Functor Versioned where
  fmap f (Versioned x) = Versioned (f x)
  fmap _ (VersionedID scID v) = VersionedID scID v

-- | Like 'Maybe'.  We don't provide a 'Monad' instance because, unlike
-- 'Maybe', 'Versioned' values shouldn't go from one constructor to the
-- other except when we say so.
instance Applicative Versioned where
  pure = Versioned
  (Versioned f) <*> (Versioned x) = Versioned (f x)
  _ <*> (VersionedID scID v) = VersionedID scID v

-- | -
instance Traversable Versioned where
  traverse f (Versioned x) = Versioned <$> f x
  traverse _ (VersionedID scID v) = pure $ VersionedID scID v

-- | Read 'Versioned' values as a 'ReadVersioned', i.e. of the form
-- @ShortContractID ::: Digest@.  Regardless of whether 'a' has a 'Read'
-- instance, @Versioned a@ always does, though the version may fail to be
-- resolved.
instance Read (Versioned a) where
  readsPrec n s = [ (VersionedID scID v, x) | (scID ::: v, x) <- readsPrec n s ]

-- | This instance enforces 'Versioned's as being opaque, unified types.
-- Ideally this designation should be accompanied by actual data hiding.
instance (Versionable a) => Versionable (Versioned a) where
  versions f (Versioned x) = (ver, emptyVersionMap) where
    (ver, _) = versions f x 

  versionMap f (Versioned x) = versionMap f x
  versionMap f (VersionedID _ ver) = throw $ UnresolvedVersionID ver

  mapVersions _ Versioned{} = throw UnexpectedResolvedVersion
  mapVersions (VersionMap' vMap) (VersionedID scID ver) =
    Versioned $ 
    fromMaybe 
      (throw $ BadVersionedType ver (bearerType xDyn) (typeRep $ Proxy @a)) $ 
    unBearer xDyn
    where 
      xDyn = 
        fromMaybe (throw $ BadVersionID scID ver) $
        Map.lookup ver $
        maybe (throw $ BadVersionID scID ver) getVersionMap $
        Map.lookup scID vMap

-- | Pass through the 'Versioned' constructor
instance (HasEscrowIDs a) => HasEscrowIDs (Versioned a) where
  traverseEscrowIDs f (Versioned x) = Versioned <$> traverseEscrowIDs f x
  traverseEscrowIDs _ (VersionedID _ ver) = throw $ UnresolvedVersionID ver

-- | This key base case actually uses the function argument to 'versions'
-- by applying it to the escrow ID.
instance (HasEscrowIDs name) => Versionable (EscrowID name) where
  versions f eID@EscrowID{..} = (f entID, emptyVersionMap) 
  mapVersions _ eID = eID

-- | -
instance Versionable Char where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Word where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Int where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Integer where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Float where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Double where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable Natural where
  versions = defaultVersions
  mapVersions = defaultMapVersions
-- | -
instance Versionable PublicKey where
  versions = defaultVersions
  mapVersions = defaultMapVersions

-- | No type has no version
instance GVersionable V1 where
  gVersions _ _ = (undefined, emptyVersionMap)
  gMapVersions _ _ = undefined

-- | Enumeration type has no version
instance GVersionable U1 where
  gVersions _ U1 = (mkVersionID (), emptyVersionMap)
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
-- constructor version.  For newtypes, we act as though we have
-- a 'Versioned' and ignore the version map of the inner type, the
-- principle being that a newtype should have a completely fresh interface
-- attached to an existing data representation.
instance 
  (GVersionable f, KnownSymbol tSym, KnownSymbol mSym, IfK nt) => 
  GVersionable (M1 D (MetaData tSym mSym p nt) f) where

  -- Same comment as for @M1 C@
  gVersions f m@(M1 x) = (vID, ifK (Proxy @nt) emptyVersionMap vers) where
    (ver, vers) = gVersions f x
    vID = mkVersionID (mName, tName, ver)
    mName = symbolVal (Proxy @mSym)
    tName = symbolVal (Proxy @tSym)

  gMapVersions vMap (M1 x) = M1 $ gMapVersions vMap x

-- | Constructor version is the hash of the constructor name with the
-- list of record versions.
instance
  (GRecords f, KnownSymbol cSym) =>
  GVersionable (M1 C (MetaCons cSym x s) f) where

  -- 'x' is only a representation type, so we don't put it in the map.
  gVersions f (M1 x) = (vID, versM) where 
    versM@(VersionMap vers) = evalState (gRecords f x) 0
    vID = mkVersionID (cName, Map.keys vers)
    cName = symbolVal (Proxy @cSym)

  gMapVersions vMap (M1 x) = M1 $ gRMapVersions vMap x

-- | Empty type has no records
instance GRecords V1 where
  gRecords _ _ = return emptyVersionMap
  gRMapVersions _ _ = undefined

-- | Type with no fields has no records
instance GRecords U1 where
  gRecords _ U1 = return emptyVersionMap
  gRMapVersions _ U1 = U1

-- | We completely ignore record selector metadata because we generate the
-- record numbering ourselves.
instance (GRecords f) => GRecords (M1 S md f) where
  gRecords f (M1 x) = gRecords f x
  gRMapVersions vMap (M1 x) = M1 $ gRMapVersions vMap x

-- | Product type records are the concatenation of the factor records.
instance (GRecords f, GRecords g) => GRecords (f :*: g) where
  gRecords f (x :*: y) = liftM2 vUnion (gRecords f x) (gRecords f y)
  gRMapVersions vMap (x :*: y) = gRMapVersions vMap x :*: gRMapVersions vMap y

-- | Recursive type is a record in itself.
instance (Versionable c) => GRecords (K1 i c) where
  gRecords f (K1 x) = do
    let (ver, VersionMap vers) = versions f x
    n <- get
    modify (+ 1)
    -- The version as a record differs from the inherent version.  This
    -- is why we don't include self in 'versions' output.
    let vID = mkVersionID (n, ver)
    -- It's important that here, we have 'x' as its actual type, not as
    -- @Rep c@, should 'c' be 'Generic'.  Therefore its 'BearsValue' is
    -- meaningful.
    return $ VersionMap $ Map.insert vID (bearer x) vers

  gRMapVersions vMap (K1 x) = K1 $ mapVersions vMap x

-- | -
instance IfK False where
  ifK _ _ x = x

-- | -
instance IfK True where
  ifK _ x _ = x

-- * Functions
-- | For default instances of 'Versionable'
defaultVersions :: 
  (Digestible a) => 
  (EntryID -> VersionID) -> a -> (VersionID, VersionMap)
defaultVersions _ x = (mkVersionID x, emptyVersionMap)

-- | For default instances of 'Versionable'
defaultMapVersions :: VersionMap' -> a -> a
defaultMapVersions _ = id

-- | For convenience
emptyVersionMap :: VersionMapT a
emptyVersionMap = VersionMap Map.empty

-- | For convenience
emptyVersionMap' :: VersionMap'
emptyVersionMap' = VersionMap' Map.empty

-- | For convenience
vUnion :: VersionMap -> VersionMap -> VersionMap
vUnion (VersionMap m1) (VersionMap m2) = VersionMap $ Map.union m1 m2

-- | Utility
mkVersionID :: (Digestible a) => a -> VersionID
mkVersionID = VersionID . digest

-- | Convenient because of the newtype
addContractVersions :: ContractID -> VersionMap -> VersionMap' -> VersionMap'
addContractVersions cID vMap (VersionMap' vers) =
  VersionMap' $ Map.insert (shorten cID) vMap vers


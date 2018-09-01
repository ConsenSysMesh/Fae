{-# LANGUAGE UndecidableInstances, PolyKinds #-}
{- |
Module: Blockchain.Fae.Internal.Serialization
Description: Serializing Fae values
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae values can be any Haskell type, and therefore are not necessarily serializable to binary.  Nonetheless, it is valuable to be able to transfer values over a network and so this module provides a generic interface to do so.  There are two main complications:

  - A value may be an inherently unserializable type like a function
  - A value must be assumed to contain escrow IDs, which are in themselves
    completely useless because they need to be accompanied by the
    associated escrow entries.

The first one is dealt with by enumerating all the "base cases", primitive types (or types with a primitive nature, like 'PublicKey') that can be serialized.

The second one is solved by replacing each escrow ID with its corresponding entry, so long as that entry is still in a state described by a 'ContractName' rather than an opaque function.  This replacement obviously can't occur without changing the type that was formerly an escrow ID, so we simulate this kind of change by operating at the level of a 'Generic'-style representation, which can be formally constructed and deconstructed.
-}
module Blockchain.Fae.Internal.Serialization where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Exceptions

import Common.Lens hiding (from, to)

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.ByteString
import Data.Maybe
import Data.Proxy
import Data.Typeable

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Serialize (Serialize, GSerializePut, GSerializeGet)
import qualified Data.Serialize as S

import GHC.Generics

import Numeric.Natural

-- | This resembles 'Generic' but uses a different representation type
-- family (because of the need to substitute 'EscrowID' fields) and
-- operates in a monadic context containing escrows (to obtain the entries
-- that will be substituted).
class EGeneric a where
  eFrom :: (MonadState EscrowMap m) => a -> m (ERep a)
  default eFrom :: (MonadState EscrowMap m, ERep a ~ a) => a -> m (ERep a)
  eFrom = return

  eTo   :: (MonadState EscrowMap m) => ERep a -> m a
  default eTo :: (MonadState EscrowMap m, ERep a ~ a) => ERep a -> m a
  eTo = return

-- | This resembles 'Generic1' with the same changes as for 'EGEneric'.
class EGeneric1 f where
  eFrom1 :: (MonadState EscrowMap m) => f p -> m (ERep1 f p)
  default eFrom1 :: (MonadState EscrowMap m, ERep1 f ~ f) => f p -> m (ERep1 f p)
  eFrom1 = return

  eTo1   :: (MonadState EscrowMap m) => ERep1 f p -> m (f p)
  default eTo1   :: (MonadState EscrowMap m, ERep1 f ~ f) => ERep1 f p -> m (f p)
  eTo1 = return

-- | This shrinks the signature of the constraint, allowing it to be used
-- /without/ @UndecidableInstances@ elsewhere.
class (Serialize (ERep a)) => ESerialize a where

-- | This family enumerates all the primitive types that represent
-- themselves, specifies the substitution for escrow IDs, and otherwise
-- defers to 'ERep1' to get the generic representation.  (Therefore, @c@
-- must have a 'Generic' instance unless it is specifically enumerated.)
type family ERep c :: * where
  ERep (EscrowID name) = EEnt name
  ERep Char = Char
  ERep Word = Word
  ERep Int = Int
  ERep Integer = Integer
  ERep Float = Float
  ERep Double = Double
  ERep Natural = Natural
  ERep PublicKey = PublicKey
  ERep Digest = Digest
  ERep c = SERep1 (Rep c)

-- | This is mostly boilerplate recursion, except for the 'K1' instance,
-- which returns control to 'ERep' to continue transforming the concrete
-- value.
type family ERep1 a :: * -> * where
  ERep1 (f :+: g) = ERep1 f :+: ERep1 g
  ERep1 (f :*: g) = ERep1 f :*: ERep1 g
  ERep1 (M1 i t f) = M1 i t (ERep1 f)
  ERep1 (K1 i c) = K1 i (ERep c)
  ERep1 U1 = U1
  ERep1 V1 = V1

-- | This is like, but not the same as an 'EscrowEntry'.  It records
-- exactly the information necessary to completely describe the escrow,
-- including its type.  The representation assumes a 'ContractName'
-- representation, so the 'escrowNameOrFunction' is flattened into the
-- fields of the 'NamedContract' that is its 'Left' variant.
--
-- If the 'EscrowEntry' contains a nonempty 'endowment', that is
-- represented in the 'EEnt's that are part of the final @'ERep' name@
-- field.  In other words, the contract name itself may have fields
-- containing escrow IDs, and these are the only escrow IDs that the escrow
-- function knows about.  The endowment is constructed in the first place
-- by extracting their backing escrows, and deconstructed here by
-- converting them to their 'ERep's.
data EEnt name = EEnt EntryID Digest VersionID (ERep name) deriving (Generic)

-- | The "S" stands for 'Serialize'; the newtype exists to give a novel
-- instance to the generic representation.  Specializing the @p@ parameter
-- to @()@ is ugly but necessary to bring the kind down to 'Type'.
newtype SERep1 f = SERep1 (ERep1 f ())

-- | -
instance (ESerialize name) => Serialize (EEnt name)

-- | Fetches and replaces escrow entries from the context.
instance (Typeable name, EGeneric name) => EGeneric (EscrowID name) where
  eFrom eID@EscrowID{..} = useNamedEscrow eID fromEscrowEntry

  eTo eEnt = do
    at entID %= Just . fromMaybe eEntry
    return eID
    where (eID@EscrowID{..}, eEntry) = toEscrowEntry eEnt

-- | Not inherently monadic (it just 'return's a pure value), this picks
-- apart the 'Left' variant of an escrow entry to create an 'EEnt'.
fromEscrowEntry :: 
  (Typeable name, EGeneric name, Monad m) => 
  EntryID -> 
  VersionID -> 
  Either (NamedContract name) AbstractLocalContract ->
  m (EEnt name)
fromEscrowEntry entID escrowVersion = either fromNamedContract (throw err) where
  err = NotStartState entID escrowVersion
  fromNamedContract NamedContract{..} = 
    return $ EEnt entID contractNextID escrowVersion eRep
    where 
      -- | Exploiting the fact that 'EGeneric' only requires a 'MonadState
      -- EscrowMap' rather than (say) a 'FaeTXM', we recursively serialize
      -- the contract name by supplying the endowment as its context.
      eRep = evalState (eFrom contractName) endowment

-- | Sorts the components of an 'EEnt' into those of an 'EscrowEntry'.
toEscrowEntry :: 
  forall name. 
  (Typeable name, EGeneric name) => 
  EEnt name -> (EscrowID name, EscrowEntry)
toEscrowEntry (EEnt entId contractNextID escrowVersion eRep) = 
  (EscrowID entId, EscrowEntry{..}) where 
    escrowNameOrFunction = Left $ AnyNamedContract @name NamedContract{..}
    -- | As in 'fromEscrowEntry', runs the 'EGeneric' method inside
    -- a plain 'State' monad and gets the final state for the endowment.
    (contractName, endowment) = runState (eTo eRep) Map.empty

-- | - 
instance EGeneric Char
-- | - 
instance EGeneric Word
-- | - 
instance EGeneric Int
-- | - 
instance EGeneric Integer
-- | - 
instance EGeneric Float
-- | - 
instance EGeneric Double
-- | - 
instance EGeneric Natural
-- | - 
instance EGeneric PublicKey
-- | - 
instance EGeneric Digest

-- | - 
instance (EGeneric1 f, EGeneric1 g) => EGeneric1 (f :+: g) where
  eFrom1 (L1 x) = L1 <$> eFrom1 x
  eFrom1 (R1 x) = R1 <$> eFrom1 x

  eTo1 (L1 x) = L1 <$> eTo1 x
  eTo1 (R1 x) = R1 <$> eTo1 x
 
-- | - 
instance (EGeneric1 f, EGeneric1 g) => EGeneric1 (f :*: g) where
  eFrom1 (x :*: y) = (:*:) <$> eFrom1 x <*> eFrom1 y
  eTo1 (x :*: y) = (:*:) <$> eTo1 x <*> eTo1 y

-- | - 
instance (EGeneric1 f) => EGeneric1 (M1 i t f) where
  eFrom1 = fmap M1 . eFrom1 . unM1
  eTo1 = fmap M1 . eTo1 . unM1

-- | - 
instance (EGeneric c) => EGeneric1 (K1 i c) where
  eFrom1 (K1 x) = K1 <$> eFrom x
  eTo1 (K1 x) = K1 <$> eTo x

-- | - 
instance EGeneric1 U1
-- | - 
instance EGeneric1 V1

-- | The possibility of writing this instance is why we use the intricate
-- 'ERep'-based substitution scheme rather than recursively descending into
-- the plain 'Rep', serializing along the way and substituting when
-- necessary without creating a new type.  The generic 'Serialize'
-- functions are either trivial (so not worth duplicating) or clever (so
-- hard to duplicate without extensive copy-pasting, and not desirable to
-- replace with something less clever), so it is worth our while to be able
-- to use them.
instance 
  (GSerializePut (ERep1 f), GSerializeGet (ERep1 f)) => Serialize (SERep1 f) where

  put (SERep1 x) = S.gPut x
  get = SERep1 <$> S.gGet


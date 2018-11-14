{-# LANGUAGE UndecidableInstances, PolyKinds #-}
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
import Data.Void

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Serialize (Serialize, GSerializePut, GSerializeGet)
import qualified Data.Serialize as S

import GHC.Generics

import Numeric.Natural

class EGeneric a where
  eFrom :: (MonadState EscrowMap m) => a -> m (ERep a)
  default eFrom :: (MonadState EscrowMap m, ERep a ~ a) => a -> m (ERep a)
  eFrom = return

  eTo   :: (MonadState EscrowMap m) => ERep a -> m a
  default eTo :: (MonadState EscrowMap m, ERep a ~ a) => ERep a -> m a
  eTo = return

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
  ERep (a -> b) = ()
  ERep (Container (t a)) = Container (t (ERep a))
  ERep c = SERep1 (Rep c)

type family ERep1 a :: * -> * where
  ERep1 (f :+: g) = ERep1 f :+: ERep1 g
  ERep1 (f :*: g) = ERep1 f :*: ERep1 g
  ERep1 (M1 i t f) = M1 i t (ERep1 f)
  ERep1 (K1 i c) = K1 i (ERep c)
  ERep1 U1 = U1
  ERep1 V1 = V1

data EEnt name = EEnt EntryID Digest VersionID (ERep name) deriving (Generic)
newtype SERep1 f = SERep1 (ERep1 f ())

instance (ESerialize name) => Serialize (EEnt name)

instance (Typeable name, EGeneric name) => EGeneric (EscrowID name) where
  eFrom eID@EscrowID{..} = useNamedEscrow eID fromEscrowEntry

  eTo eEnt = do
    at entID %= Just . fromMaybe eEntry
    return eID
    where (eID@EscrowID{..}, eEntry) = toEscrowEntry eEnt

fromEscrowEntry :: 
  (Typeable name, EGeneric name, Monad m) => 
  EntryID -> 
  VersionID -> 
  Either (NamedContract name) AbstractLocalContract ->
  m (EEnt name)
fromEscrowEntry entID escrowVersion = 
  -- The positioning of this 'return' is important: it needs to be on the
  -- far outside, not in (say) 'fromNamedContract', because it needs to be
  -- unambiguous that 'fromEscrowEntry' does /not/ change the monad state,
  -- or else the exception may be thrown in trying to figure this out.
  return . either fromNamedContract (throw err) 
  where
    err = NotStartState entID escrowVersion
    fromNamedContract NamedContract{..} = 
      EEnt entID contractNextID escrowVersion eRep
      where eRep = evalState (eFrom contractName) endowment

toEscrowEntry :: 
  forall name. 
  (Typeable name, EGeneric name) => 
  EEnt name -> (EscrowID name, EscrowEntry)
toEscrowEntry (EEnt entId contractNextID escrowVersion eRep) = 
  (EscrowID entId, EscrowEntry{..}) where 
    escrowNameOrFunction = Left $ AnyNamedContract @name NamedContract{..}
    (contractName, endowment) = runState (eTo eRep) Map.empty

instance EGeneric Char
instance EGeneric Word
instance EGeneric Int
instance EGeneric Integer
instance EGeneric Float
instance EGeneric Double
instance EGeneric Natural
instance EGeneric PublicKey
instance EGeneric Digest

instance (Typeable a, Typeable b) => EGeneric (a -> b) where
  eFrom f = return $ throw $ NotExportable $ typeOf f
  eTo _ = return $ throw $ NotExportable $ typeRep $ Proxy @(a -> b)

instance (EGeneric a, Traversable t) => EGeneric (Container (t a)) where
  eFrom = fmap Container . traverse eFrom . getContainer
  eTo = fmap Container . traverse eTo . getContainer

instance (EGeneric1 f, EGeneric1 g) => EGeneric1 (f :+: g) where
  eFrom1 (L1 x) = L1 <$> eFrom1 x
  eFrom1 (R1 x) = R1 <$> eFrom1 x

  eTo1 (L1 x) = L1 <$> eTo1 x
  eTo1 (R1 x) = R1 <$> eTo1 x
 
instance (EGeneric1 f, EGeneric1 g) => EGeneric1 (f :*: g) where
  eFrom1 (x :*: y) = (:*:) <$> eFrom1 x <*> eFrom1 y
  eTo1 (x :*: y) = (:*:) <$> eTo1 x <*> eTo1 y

instance (EGeneric1 f) => EGeneric1 (M1 i t f) where
  eFrom1 = fmap M1 . eFrom1 . unM1
  eTo1 = fmap M1 . eTo1 . unM1

instance (EGeneric c) => EGeneric1 (K1 i c) where
  eFrom1 (K1 x) = K1 <$> eFrom x
  eTo1 (K1 x) = K1 <$> eTo x

instance EGeneric1 U1
instance EGeneric1 V1

instance 
  (GSerializePut (ERep1 f), GSerializeGet (ERep1 f)) => Serialize (SERep1 f) where

  put (SERep1 x) = S.gPut x
  get = SERep1 <$> S.gGet


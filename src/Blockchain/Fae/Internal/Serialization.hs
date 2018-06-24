{-# LANGUAGE UndecidableInstances, PolyKinds #-}
module Blockchain.Fae.Internal.Serialization where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Exceptions

import Common.Lens hiding (from, to)

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class

import Data.ByteString
import Data.Maybe
import Data.Proxy

import qualified Data.Map as Map

import Data.Serialize (Serialize, GSerializePut, GSerializeGet)
import qualified Data.Serialize as S

import GHC.Generics

import Numeric.Natural

class EGeneric a where
  eFrom :: (MonadState Escrows m) => a -> m (ERep a)
  default eFrom :: (MonadState Escrows m, ERep a ~ a) => a -> m (ERep a)
  eFrom = return

  eTo   :: (MonadState Escrows m) => ERep a -> m a
  default eTo :: (MonadState Escrows m, ERep a ~ a) => ERep a -> m a
  eTo = return

class EGeneric1 f where
  eFrom1 :: (MonadState Escrows m) => f p -> m (ERep1 f p)
  default eFrom1 :: (MonadState Escrows m, ERep1 f ~ f) => f p -> m (ERep1 f p)
  eFrom1 = return

  eTo1   :: (MonadState Escrows m) => ERep1 f p -> m (f p)
  default eTo1   :: (MonadState Escrows m, ERep1 f ~ f) => ERep1 f p -> m (f p)
  eTo1 = return

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

type family ERep1 a :: * -> * where
  ERep1 (f :+: g) = ERep1 f :+: ERep1 g
  ERep1 (f :*: g) = ERep1 f :*: ERep1 g
  ERep1 (M1 i t f) = M1 i t (ERep1 f)
  ERep1 (K1 i c) = K1 i (ERep c)
  ERep1 U1 = U1
  ERep1 V1 = V1

data EEnt name = EEnt (EscrowID name) EscrowEntry
newtype SERep1 f = SERep1 (ERep1 f ())

instance (Typeable name) => EGeneric (EscrowID name) where
  eFrom eID = EEnt eID <$> peekEscrow eID
  eTo (EEnt eID@EscrowID{..} entry) = do
    _escrowMap . at entID %= Just . fromMaybe entry
    return eID

instance EGeneric Char
instance EGeneric Word
instance EGeneric Int
instance EGeneric Integer
instance EGeneric Float
instance EGeneric Double
instance EGeneric Natural
instance EGeneric PublicKey
instance EGeneric Digest

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

instance (ContractName name, Serialize name) => Serialize (EEnt name) where
  put (EEnt EscrowID{entID} EscrowEntry{..}) = 
    case escrowNameOrFunction of
      Left c -> do
        let NamedContract{..} = nameContract @name entID c
        unless (Map.null endowment) $ throw $ HoldsEscrows entID
        S.put (entID, escrowVersion, contractName)
      Right _ -> throw $ NotStartState entID escrowVersion

  get = do
    (entID, escrowVersion, contractName :: name) <- S.get
    let endowment = Map.empty
        escrowNameOrFunction = Left $ AnyNamedContract NamedContract{..}
    return $ EEnt EscrowID{..} EscrowEntry{..}

instance 
  (GSerializePut (ERep1 f), GSerializeGet (ERep1 f)) => Serialize (SERep1 f) where

  put (SERep1 x) = S.gPut x
  get = SERep1 <$> S.gGet


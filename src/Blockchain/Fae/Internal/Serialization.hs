{-# LANGUAGE UndecidableInstances, PolyKinds #-}
module Blockchain.Fae.Internal.Serialization where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Exceptions

import Common.Lens hiding (from, to)

import Control.Applicative
import Control.Monad

import Data.ByteString
import Data.Maybe
import Data.Proxy

import qualified Data.Map as Map

import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import qualified Data.Serialize.Put as S
import qualified Data.Serialize.Get as S

import GHC.Generics

type family ERep (a :: * -> *) :: * -> * where
  -- This is awful, but safe, because the last parameter of (E)Rep is ignored.
  ERep (K1 i c) = K1 i (Either (EEnt c) (ERep (Rep c) ()))
  ERep (f :+: g) = ERep f :+: ERep g
  ERep (f :*: g) = ERep f :*: ERep g
  ERep (M1 i t f) = M1 i t (ERep f)
  ERep U1 = U1
  ERep V1 = V1

data family EEnt c 

data instance EEnt (EscrowID name) = 
  EEnt
  {
    eEntID :: EscrowID name,
    eEntry :: EscrowEntry
  }

class EGeneric f where
  eFrom :: f p -> FaeTXM (ERep f p)
  eTo   :: ERep f p -> FaeTXM (f p)

instance {-# OVERLAPPING #-} 
  (Typeable name) => EGeneric (K1 i (EscrowID name)) where

  eFrom (K1 eID) = K1 . Left . EEnt eID <$> peekEscrow eID
  eTo (K1 (Left (EEnt eID@EscrowID{..} entry))) = K1 <$> do
    _escrowMap . at entID %= Just . fromMaybe entry
    return eID
  eTo _ = throw BadEGeneric

instance (Generic c, EGeneric (Rep c)) => EGeneric (K1 i c) where
  eFrom = fmap (K1 . Right) . eFrom . from . unK1
  eTo (K1 (Right x)) = K1 . to <$> eTo x
  eTo _ = throw BadEGeneric

instance (EGeneric f, EGeneric g) => EGeneric (f :+: g) where
  eFrom (L1 x) = L1 <$> eFrom x
  eFrom (R1 x) = R1 <$> eFrom x

  eTo (L1 x) = L1 <$> eTo x
  eTo (R1 x) = R1 <$> eTo x
 
instance (EGeneric f, EGeneric g) => EGeneric (f :*: g) where
  eFrom (x :*: y) = (:*:) <$> eFrom x <*> eFrom y
  eTo (x :*: y) = (:*:) <$> eTo x <*> eTo y

instance (EGeneric f) => EGeneric (M1 i t f) where
  eFrom = fmap M1 . eFrom . unM1
  eTo = fmap M1 . eTo . unM1

instance EGeneric U1 where
  eFrom = return
  eTo = return

instance EGeneric V1 where
  eFrom = return
  eTo = return

instance (ContractName name) => Serialize (EEnt (EscrowID name)) where
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

exportValue :: 
  -- Again, awful but safe
  (Generic a, EGeneric (Rep a), Serialize (ERep (Rep a) ())) => 
  a -> FaeTXM ByteString
exportValue = fmap S.encode . eFrom . from @_ @()

importValue :: 
  -- Again, awful but safe
  (Generic a, EGeneric (Rep a), Serialize (ERep (Rep a) ())) =>
  ByteString -> FaeTXM (Maybe a)
importValue = 
  either (const $ return Nothing) (fmap (Just . to @_ @()) . eTo) . S.decode 


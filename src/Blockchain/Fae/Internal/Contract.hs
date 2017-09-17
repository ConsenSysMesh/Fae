{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto 
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Monads

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Fix
import Control.Monad.RWS hiding ((<>))
import Control.Monad.State
import Control.Monad.Trans

import Data.Coerce
import Data.Dynamic
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Proxy

{- API functions -}

data BearsValue = forall a. (HasEscrowIDs a) => BearsValue a

bearer :: (HasEscrowIDs a) => a -> BearsValue
bearer x = BearsValue x

newtype Fae argType valType a = Fae { getFae :: FaeM argType valType a }
  deriving (Functor, Applicative, Monad)

sender :: (MonadFae argType valType m) => m PublicKey
sender = liftFae $ Fae $ lift $ lift $ Wrapped ask

type Contract m argType valType = 
  (MonadFae argType valType m) => 
  argType -> m (WithEscrows valType)

type Contract' argType valType = Contract (Fae argType valType) argType valType

type AnyFae a = forall argType valType m. (MonadFae argType valType m) => m a

class (MonadFae argType valType m) => MonadFae' argType valType m where
  release :: valType -> m argType

class 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadContracts m) => 
  MonadFae argType valType m | m -> argType valType where

  spend :: valType -> m (WithEscrows valType)
  liftFae :: Fae argType valType a -> m a

class (Monad m) => MonadContracts m where
  useEscrow :: 
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    EscrowID argType valType -> argType -> m valType
  newEscrow :: 
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    [BearsValue] -> Contract' argType valType -> m (EscrowID argType valType)
  newContract ::
    (
      HasEscrowIDs argType, HasEscrowIDs valType,
      Typeable argType, Typeable valType
    ) =>
    [BearsValue] -> [ShortContractID] -> Contract' argType valType -> m ()

instance 
  (MonadTrans t, MonadFae' argType valType m, Monad (t m)) =>
  MonadFae' argType valType (t m) where

  release = lift . release

instance 
  (MonadTrans t, MonadFae argType valType m, Monad (t m)) =>
  MonadFae argType valType (t m) where

  spend = lift . spend
  liftFae = lift . liftFae

instance 
  (MonadTrans t, MonadContracts m, Monad (t m)) => 
  MonadContracts (t m) where

  useEscrow eID arg = lift $ useEscrow eID arg
  newEscrow xs c = lift $ newEscrow xs c
  newContract xs trusts c = lift $ newContract xs trusts c

instance MonadFae' argType valType (Fae argType valType) where
  release x = do
    req <- spend x
    Fae $ suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
      lift $ modify $ Map.union inputEscrows
      return y

instance MonadFae argType valType (Fae argType valType) where
  spend = Fae . internalSpend
  liftFae = id

instance MonadContracts (Fae argType valType) where
  useEscrow (EscrowID eID) x = Fae $ do
    fAbs <- use $ at eID . defaultLens (throw $ BadEscrowID eID)
    let ConcreteContract f = unmakeAbstract fAbs
    (gAbsM, y) <- f x
    at eID .= gAbsM
    return y

  newEscrow eIDs f = Fae $ internalNewEscrow eIDs f

  newContract eIDs trusts f = Fae $ do
    cAbs <- makeContract eIDs f
    lift $ lift $ Wrapped $ 
      tell $ Seq.singleton $ TrustContract cAbs trusts

{- Internal functions -}

internalSpend :: 
  (HasEscrowIDs valType, Functor s) =>
  valType -> FaeContractStateT s (WithEscrows valType)
internalSpend x = do
  outputEscrows <- takeEscrows [bearer x]
  return $ WithEscrows outputEscrows x

internalNewEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsValue] -> Contract' argType valType -> 
  FaeContractStateT s (EscrowID argType valType)
internalNewEscrow eIDs f = do
  cAbs <- makeContract eIDs f
  eID <- lift $ lift $ Wrapped $ do
    eID <- get
    _2 += 1
    return eID
  modify $ Map.insert eID cAbs
  return $ EscrowID eID


type InternalContract argType valType =
  WithEscrows argType -> 
  FaeContractRWST (FaeRequest argType valType) (WithEscrows valType)

makeContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsValue] ->
  Contract' argType valType ->
  FaeContractStateT s AbstractContract
makeContract eIDs f = makeAbstract . makeConcrete <$> makeInternal eIDs f

makeInternal ::
  (HasEscrowIDs argType, HasEscrowIDs valType, Functor s) =>
  [BearsValue] ->
  Contract' argType valType ->
  FaeContractStateT s (InternalContract argType valType)
makeInternal eIDs f = do
  escrows <- takeEscrows eIDs
  return $ \(WithEscrows inputEscrows x) ->
    let totalEscrows = escrows `Map.union` inputEscrows in 
    mapMonad (unWrapped . flip evalStateT totalEscrows) $ getFae $ f x

makeConcrete ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete f = ConcreteContract $ \x -> do
  xE <- internalSpend x
  result <- lift $ lift $ Wrapped $ resume $ f xE
  let 
    (gM, WithEscrows outputEscrows z) =
      case result of
        Left (Request y g) -> (Just g, y)
        Right y -> (Nothing, y)
  modify $ Map.union outputEscrows
  let gAbsM = makeAbstract . makeConcrete <$> gM
  return (gAbsM, z)

makeAbstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract 
makeAbstract (ConcreteContract f) = ConcreteContract $ \xDyn -> do
  let 
    x = fromDyn xDyn $
      throw $ BadArgType (typeRep (Proxy @argType)) (dynTypeRep xDyn)
  (gM, y) <- f x
  return (makeAbstract <$> gM, toDyn y)

unmakeAbstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  AbstractContract -> ConcreteContract argType valType
unmakeAbstract (ConcreteContract f) = ConcreteContract $ \x -> do
  (gAbsM, yDyn) <- f $ toDyn x
  let 
    y = fromDyn yDyn $
      throw $ BadValType (typeRep (Proxy @valType)) (dynTypeRep yDyn)
  return (unmakeAbstract <$> gAbsM, y)

takeEscrows :: (MonadState Escrows m) => [BearsValue] -> m Escrows
takeEscrows xs = 
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> 
  forM (getEscrowIDs x) $ \(AnEscrowID (EscrowID k)) -> do
    m <- get
    modify $ Map.delete k
    return (k, m Map.! k)


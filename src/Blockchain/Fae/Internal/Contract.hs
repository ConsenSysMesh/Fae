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

release :: 
  (HasEscrowIDs valType) =>
  valType -> Fae argType valType argType
release x = do
  req <- spend x
  Wrapped $ suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
    lift $ modify $ Map.union inputEscrows
    return y

spend :: 
  (HasEscrowIDs valType) =>
  valType -> AnyFae (WithEscrows valType)
spend x = Wrapped $ do
  outputEscrows <- takeEscrows [bearer x]
  return $ WithEscrows outputEscrows x

useEscrow ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  EscrowID argType valType ->
  argType -> AnyFae valType
useEscrow (EscrowID eID) x = Wrapped $ do
  fAbs <- use $ at eID . defaultLens (throw $ BadEscrowID eID)
  let ConcreteContract f = unmakeAbstract fAbs
  (gAbsM, y) <- f x
  at eID .= gAbsM
  return y

newEscrow ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  [BearsEscrowIDs] ->
  Contract argType valType ->
  AnyFae (EscrowID argType valType)
newEscrow eIDs f = Wrapped $ do
  cAbs <- makeContract eIDs f
  eID <- lift $ lift $ Wrapped $ do
    eID <- get
    _2 += 1
    return eID
  modify $ Map.insert eID cAbs
  return $ EscrowID eID

newContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  [BearsEscrowIDs] ->
  [ShortContractID] ->
  Contract argType valType ->
  AnyFae ()
newContract eIDs trusts f = Wrapped $ do
  cAbs <- makeContract eIDs f
  lift $ lift $ Wrapped $ 
    tell $ Seq.singleton $ TrustContract cAbs trusts

{- Internal functions -}

type InternalContract argType valType =
  WithEscrows argType -> 
  FaeContractRWST (FaeRequest argType valType) (WithEscrows valType)

makeContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsEscrowIDs] ->
  Contract argType valType ->
  FaeContractStateT s AbstractContract
makeContract eIDs f = makeAbstract . makeConcrete <$> makeInternal eIDs f

makeInternal ::
  (HasEscrowIDs argType, HasEscrowIDs valType, Functor s) =>
  [BearsEscrowIDs] ->
  Contract argType valType ->
  FaeContractStateT s (InternalContract argType valType)
makeInternal eIDs f = do
  escrows <- takeEscrows eIDs
  return $ \(WithEscrows inputEscrows x) ->
    let totalEscrows = escrows `Map.union` inputEscrows in 
    mapMonad (unWrapped . flip evalStateT totalEscrows) $ unWrapped $ f x

makeConcrete ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete f = ConcreteContract $ \x -> do
  xE <- unWrapped $ spend x
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

takeEscrows :: (MonadState Escrows m) => [BearsEscrowIDs] -> m Escrows
takeEscrows xs = 
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsEscrowIDs x) -> 
  forM (getEscrowIDs x) $ \(AnEscrowID (EscrowID k)) -> do
    m <- get
    modify $ Map.delete k
    return (k, m Map.! k)


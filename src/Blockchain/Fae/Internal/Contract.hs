{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage

import Control.Monad.RWS
import Control.Monad.State

import Data.Dynamic
import Data.Map (Map)
import Data.Typeable

import qualified Data.Map as Map

{- Types -}

type Escrows = Map EntryID AbstractEscrow
data WithEscrows a = WithEscrows Escrows a
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

newtype Wrapped m a = Wrapped { unWrapped :: m a }
  deriving (Functor, Applicative, Monad)

type FaeRWS = RWS PublicKey [AbstractContract] EntryID 
type FaeContract s = Coroutine s (StateT Escrows (Wrapped FaeRWS))
newtype FaeM s a = Fae { getFae :: FaeContract s a }
  deriving (Functor, Applicative, Monad)

-- | The monad for multi-stage contracts taking an 'argType' and returning
-- a 'valType'.  These may not be transactions.
type Fae argType valType = FaeM (FaeRequest argType valType)
-- | The monad for single-stage contracts, i.e. transactions.
type FaeTX = FaeM Naught

-- | A "contract transformer", for use with the 'MonadContract' and
-- 'MonadTX' typeclasses.
type ContractT m argType valType = argType -> m (WithEscrows valType)
-- | This is the type to use when defining a new contract.  Its argument is
-- the argument given in the first call to this contract; all subsequent
-- calls pass in their arguments via 'release'.  Because a 'ContractT' must
-- return a 'WithEscrows', the contract code must ultimately terminate with
-- a 'spend', being the only API function returning this type.
type Contract argType valType = ContractT (Fae argType valType) argType valType
-- | Likewise, this type defines a new transactional escrow or actual
-- transaction.  Its argument is the argument to the only call of this
-- transaction, which cannot use 'release' but only 'spend'.
type TXEscrow argType valType = ContractT FaeTX argType valType

type InternalT s argType valType = 
  ContractT (Coroutine s FaeRWS) argType valType

type InternalContract argType valType = 
  InternalT (FaeRequest argType valType) (WithEscrows argType) valType
type InternalTX argType valType = 
  InternalT Naught (WithEscrows argType) valType

newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContract s (Maybe (ConcreteContract argType valType), valType)
  )

type AbstractEscrow = ConcreteContract Dynamic Dynamic
type AbstractContract = ConcreteContract String Dynamic

{- Orphan instances -}

deriving instance (Monoid w, MonadWriter w m) => MonadWriter w (Wrapped m)

{- Functions -}

makeEscrow ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsValue] ->
  Contract argType valType ->
  FaeContract s AbstractEscrow
makeEscrow eIDs f = 
  makeEscrowAbstract . makeEscrowConcrete <$> makeInternalT eIDs (getFae . f)

makeTXEscrow ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsValue] ->
  TXEscrow argType valType ->
  FaeContract s AbstractEscrow
makeTXEscrow eIDs f = 
  makeEscrowAbstract . makeTXEscrowConcrete <$> makeInternalT eIDs (getFae . f)

makeContract ::
  (HasEscrowIDs valType, Read argType, Typeable valType, Functor s) =>
  [BearsValue] ->
  Contract argType valType ->
  FaeContract s AbstractContract
makeContract eIDs f =
  makeContractAbstract . makeContractConcrete <$> makeInternalT eIDs (getFae . f)

makeInternalT ::
  (Functor s, Functor s') =>
  [BearsValue] ->
  ContractT (FaeContract s') argType valType ->
  FaeContract s (InternalT s' (WithEscrows argType) valType)
makeInternalT eIDs f = do
  escrows <- takeEscrows eIDs
  return $ \(WithEscrows inputEscrows x) ->
    let totalEscrows = escrows `Map.union` inputEscrows in 
    mapMonad (unWrapped . flip evalStateT totalEscrows) $ f x

makeEscrowConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeEscrowConcrete = makeConcrete internalSpend

makeTXEscrowConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  InternalTX argType valType ->
  ConcreteContract argType valType
makeTXEscrowConcrete f = ConcreteContract $ \x -> do
  xE <- internalSpend x
  WithEscrows outputEscrows z <- lift $ lift $ Wrapped $ runCoroutine $ f xE
  modify $ Map.union outputEscrows
  return (Nothing, z)

makeContractConcrete ::
  (HasEscrowIDs valType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeContractConcrete = makeConcrete $ return . WithEscrows Map.empty

makeConcrete ::
  (HasEscrowIDs valType) =>
  (forall s. (Functor s) => argType -> FaeContract s (WithEscrows argType)) ->
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete mkE f = ConcreteContract $ \x -> do
  xE <- mkE x
  result <- lift $ lift $ Wrapped $ resume $ f xE
  let 
    (gM, WithEscrows outputEscrows z) =
      case result of
        Left (Request y g) -> (Just g, y)
        Right y -> (Nothing, y)
  modify $ Map.union outputEscrows
  let gConcM = makeContractConcrete <$> gM
  return (gConcM, z)

makeEscrowAbstract ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractEscrow 
makeEscrowAbstract (ConcreteContract f) = ConcreteContract $ \xDyn -> do
  let 
    x = fromDyn xDyn $
      throw $ BadArgType (typeRep (Proxy @argType)) (dynTypeRep xDyn)
  (gM, y) <- f x
  return (makeEscrowAbstract <$> gM, toDyn y)

makeContractAbstract ::
  forall argType valType.
  (Read argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract f) = ConcreteContract $ \str -> do
  let x = read str
  (gM, y) <- f x
  return (makeContractAbstract <$> gM, toDyn y)

unmakeAbstractEscrow ::
  forall argType valType.
  (Typeable argType, Typeable valType) =>
  AbstractEscrow -> ConcreteContract argType valType
unmakeAbstractEscrow (ConcreteContract f) = ConcreteContract $ \x -> do
  (gAbsM, yDyn) <- f $ toDyn x
  let 
    y = fromDyn yDyn $
      throw $ BadValType (typeRep (Proxy @valType)) (dynTypeRep yDyn)
  return (unmakeAbstractEscrow <$> gAbsM, y)

takeEscrows :: (MonadState Escrows m) => [BearsValue] -> m Escrows
takeEscrows xs = 
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> 
  forM (getEscrowIDs x) $ \(AnEscrowID (EscrowID k)) -> do
    m <- get
    modify $ Map.delete k
    return (k, m Map.! k)

internalSpend :: 
  (HasEscrowIDs valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  outputEscrows <- takeEscrows [bearer x]
  return $ WithEscrows outputEscrows x


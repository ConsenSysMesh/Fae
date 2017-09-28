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

type Escrows = Map EntryID AbstractContract
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

type InternalContract argType valType =
  WithEscrows argType -> 
  Coroutine (FaeRequest argType valType) FaeRWS (WithEscrows valType)

newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContract s (Maybe (ConcreteContract argType valType), valType)
  )

type AbstractContract = ConcreteContract Dynamic Dynamic

-- | A "contract transformer", for use with the 'MonadContract' and
-- 'MonadTX' typeclasses.
type ContractT m argType valType = argType -> m (WithEscrows valType)
-- | This is the type to use when defining a new contract.  Its argument is
-- the argument given in the first call to this contract; all subsequent
-- calls pass in their arguments via 'release'.  Because a 'ContractT' must
-- return a 'WithEscrows', the contract code must ultimately terminate with
-- a 'spend', being the only API function returning this type.
type Contract argType valType = ContractT (Fae argType valType) argType valType

{- Orphan instances -}

deriving instance (Monoid w, MonadWriter w m) => MonadWriter w (Wrapped m)

{- Functions -}

makeContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  [BearsValue] ->
  Contract argType valType ->
  FaeContract s AbstractContract
makeContract eIDs f = makeAbstract . makeConcrete <$> makeInternal eIDs f

makeInternal ::
  (HasEscrowIDs argType, HasEscrowIDs valType, Functor s) =>
  [BearsValue] ->
  Contract argType valType ->
  FaeContract s (InternalContract argType valType)
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
  let gConcM = makeConcrete <$> gM
  return (gConcM, z)

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

internalSpend :: 
  (HasEscrowIDs valType, Functor s) => 
  valType -> FaeContract s (WithEscrows valType)
internalSpend x = do
  outputEscrows <- takeEscrows [bearer x]
  return $ WithEscrows outputEscrows x


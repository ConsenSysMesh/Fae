{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq
import Control.Monad.RWS
import Control.Monad.State

import Data.Dynamic
import Data.Functor.Const
import Data.Map (Map)
import Data.Typeable

import qualified Data.Map as Map

{- Types -}

type Escrows = Map EntryID AbstractEscrow
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via `spend` and `release`.
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
-- calls pass in their arguments via 'release'.
type Contract argType valType = ContractT (Fae argType valType) argType valType

type InternalT s argType valType = 
  ContractT (Coroutine s FaeRWS) argType valType

type InternalContract argType valType = 
  InternalT (FaeRequest argType valType) (WithEscrows argType) valType

newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContract s (Maybe (ConcreteContract argType valType), valType)
  )

type AbstractEscrow = ConcreteContract Dynamic Dynamic
type AbstractContract = ConcreteContract String Dynamic

{- Instances -}

deriving instance (Monoid w, MonadWriter w m) => MonadWriter w (Wrapped m)

instance NFData (ConcreteContract argType valType) where
  rnf (ConcreteContract !f) = ()

{- Functions -}

makeEscrow ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType, NFData argType,
    Functor s
  ) =>
  [BearsValue] ->
  Contract argType valType ->
  FaeContract s AbstractEscrow
makeEscrow eIDs f = 
  makeEscrowAbstract . makeEscrowConcrete <$> makeInternalT eIDs (getFae . f)

makeContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType, 
    Read argType, Typeable argType, Typeable valType, 
    Functor s
  ) =>
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
  (HasEscrowIDs argType, HasEscrowIDs valType, NFData argType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeEscrowConcrete = makeConcrete internalSpend

makeContractConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeContractConcrete = makeConcrete $ return . WithEscrows Map.empty

makeConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  (forall s. (Functor s) => argType -> FaeContract s (WithEscrows argType)) ->
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete mkE f = ConcreteContract $ \x -> do
  x' <- runTXEscrows x
  xE <- mkE x'
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
  (Read argType, Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract f) = ConcreteContract $ \str -> do
  let 
    x =
      case [y | (y, "") <- reads str] of
        [y] -> y
        _ -> throw $ BadInputParse str $ typeRep (Proxy @argType)
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

takeEscrows :: forall m. (MonadState Escrows m) => [BearsValue] -> m Escrows
takeEscrows xs = 
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> do
    let
      f :: EscrowIDMap (Const [m (EntryID, AbstractEscrow)])
      f eID = 
        Const 
        [do
          let k = entID eID
          m <- get
          modify $ Map.delete k
          return (k, m Map.! k)
        ]
    sequence $ getConst $ traverseEscrowIDs f x

runTXEscrows :: forall a s. (HasEscrowIDs a, Functor s) => a -> FaeContract s a
runTXEscrows x = traverseEscrowIDs f x where
  f :: EscrowIDMap (FaeContract s)
  f (TXEscrowIn eID arg) = TXEscrowOut eID <$> internalUseEscrow eID arg
  f eID = return eID

internalSpend :: 
  (HasEscrowIDs valType, MonadState Escrows m, NFData valType) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  outputEscrows <- takeEscrows [bearer x]
  -- We need to force both the escrows and the value because they may have
  -- nonterminating computations in them, which we want to be suffered by
  -- the originating contract and not by its victims.
  return $ WithEscrows (force outputEscrows) (force x)

internalUseEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  EntryID -> argType -> FaeContract s valType
internalUseEscrow entID x = do
  fAbs <- use $ at entID . defaultLens (throw $ BadEscrowID entID)
  let ConcreteContract f = unmakeAbstractEscrow fAbs
  (gConcM, y) <- f x
  at entID .= fmap makeEscrowAbstract gConcM
  return y


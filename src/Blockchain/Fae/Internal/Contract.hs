{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Coroutine hiding (Reader)
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Dynamic
import Data.Functor.Const
import Data.Map (Map)
import Data.Typeable
import Data.Void

import qualified Data.Map as Map

import GHC.Generics

{- Types -}

data Escrows =
  Escrows
  {
    escrowMap :: EscrowMap,
    nextID :: EntryID
  } deriving (Generic)
type EscrowMap = Map EntryID AbstractEscrow
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via `spend` and `release`.
data WithEscrows a = WithEscrows EscrowMap a
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

type FaeRW = WriterT [AbstractContract] (Reader PublicKey) 

type FaeContract s = Coroutine s (StateT Escrows FaeRW)
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
  ContractT (Coroutine s FaeRW) argType valType

type InternalContract argType valType = 
  InternalT (FaeRequest argType valType) (WithEscrows argType) valType

newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContract s (Maybe (ConcreteContract argType valType), valType)
  )

type AbstractEscrow = ConcreteContract Dynamic Dynamic
type AbstractContract = ConcreteContract Input Dynamic

-- Exception type
data ContractException =
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID
  deriving (Typeable, Show)

data Input = forall a. (HasEscrowIDs a) => Input a String

{- Typeclasses -}

-- | For reading contract arguments as transaction inputs.
class ReadInput a where
  readInput :: Input -> a
  default readInput :: (Read a, Typeable a) => Input -> a
  readInput (Input x s) =
    case filter (null . snd) $ reads s of
      [(y, "")] -> resolveEscrowLocators x y
      _ -> throw $ BadInputParse s (typeRep $ Proxy @a)

{- Instances -}

instance Exception ContractException

instance NFData Escrows
instance NFData (ConcreteContract argType valType) where
  rnf (ConcreteContract !f) = ()

instance ReadInput Void
instance ReadInput ()
instance ReadInput Bool
instance ReadInput Char
instance ReadInput Int
instance ReadInput Integer
instance ReadInput Float
instance ReadInput Double
instance (Read a, Typeable a) => ReadInput [a]
instance (Read a, Typeable a) => ReadInput (Maybe a)
instance (Read a, Typeable a, Read b, Typeable b) => ReadInput (a, b)
instance (Read a, Typeable a, Read b, Typeable b) => ReadInput (Either a b)
instance 
  (Typeable argType, Typeable valType) =>
  ReadInput (EscrowID argType valType)

{- TH -}

makeLenses ''Escrows

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
    ReadInput argType, Typeable argType, Typeable valType,
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
  Escrows{..} <- takeEscrows eIDs
  let seedID = digest (nextID, nextID)
  _nextID %= digest
  entID <- use _nextID
  return $ \(WithEscrows inputEscrows x) ->
    mapMonad (flip evalStateT Escrows{nextID = seedID, ..}) $ do
      _escrowMap %= Map.union inputEscrows
      f x

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
  result <- lift $ lift $ resume $ f xE
  let 
    (gM, WithEscrows outputEscrows z) =
      case result of
        Left (Request y g) -> (Just g, y)
        Right y -> (Nothing, y)
  _escrowMap %= Map.union outputEscrows
  let gConcM = makeConcrete mkE <$> gM
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
  (ReadInput argType, Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract f) = ConcreteContract $ \input -> do
  (gM, y) <- f $ readInput input
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
takeEscrows xs = do
  escrowMap <- getEscrowMap xs
  nextID <- use _nextID
  return Escrows{..}

getEscrowMap :: forall m. (MonadState Escrows m) => [BearsValue] -> m EscrowMap
getEscrowMap xs =
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> do
    let
      f :: EscrowIDMap (Writer [m (EntryID, AbstractEscrow)])
      f eID@EscrowLocator{} = return eID
      f eID = tell [takeEscrow eID] >> return eID
    sequence $ execWriter $ traverseEscrowIDs f x

takeEscrow :: (MonadState Escrows m) => 
  EscrowID argType valType -> m (EntryID, AbstractEscrow)
takeEscrow EscrowLocator{..} = throw $ UnresolvedEscrowLocator path
takeEscrow eID = do
  let k = entID eID
  xM <- use $ _escrowMap . at k
  case xM of
    Just x -> do
      _escrowMap . at k .= Nothing
      return (k, x)
    Nothing -> throw $ BadEscrowID $ entID eID

internalSpend :: 
  (HasEscrowIDs valType, MonadState Escrows m, NFData valType) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  Escrows{..} <- takeEscrows [bearer x]
  -- We need to force both the escrows and the value because they may have
  -- nonterminating computations in them, which we want to be suffered by
  -- the originating contract and not by its victims.
  return $ WithEscrows (force escrowMap) (force x)

internalUseEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s
  ) =>
  EntryID -> argType -> FaeContract s valType
internalUseEscrow entID x = do
  fAbs <- use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  let ConcreteContract f = unmakeAbstractEscrow fAbs
  (gConcM, y) <- f x
  _escrowMap . at entID .= fmap makeEscrowAbstract gConcM
  return y

runTXEscrows :: forall a s. (HasEscrowIDs a, Functor s) => a -> FaeContract s a
runTXEscrows = traverseEscrowIDs f where
  f :: EscrowIDMap (FaeContract s)
  f (TXEscrowIn eID arg) = TXEscrowOut eID <$> internalUseEscrow eID arg
  f eID = return eID


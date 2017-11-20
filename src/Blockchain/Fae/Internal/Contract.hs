{- |
Module: Blockchain.Fae.Internal.Contract
Description: The core 'Contract' type that underlies Fae
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Everything in Fae is a contract, and this module defines contracts and their construction.
-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Coroutine hiding (Reader)
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Dynamic
import Data.Functor.Const
import Data.Functor.Identity
import Data.Map (Map)
import Data.Maybe
import Data.Typeable
import Data.Void

import qualified Data.Map as Map

import GHC.Generics

import Text.Read (readMaybe)

-- * Types

-- | How escrows are kept in each contract.  Contracts track their escrow
-- ID sequence separately to prevent coupling between different ones.
data Escrows =
  Escrows
  {
    escrowMap :: EscrowMap,
    nextID :: EntryID
  } deriving (Generic)
-- | Convenience type.  Each escrow has a continually updated version that
-- we need to track; this holds both for the escrow map of a contract and
-- the escrows accompanying a returned value.
type EscrowMap = Map EntryID (AbstractEscrow, VersionID)
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via 'spend' and 'release'.
data WithEscrows a = WithEscrows EscrowMap a
-- | The 'Request' type is backwards for my visualization of contracts as
-- coroutines, and also needs to be augmented with backing escrows.
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

-- | This monad contains everything that relates a contract to its
-- surrounding transaction.
type FaeRWT m = WriterT [AbstractContract] (ReaderT TXData m)
-- | The commonly used variant
type FaeRW = FaeRWT Identity
-- | The relevant transaction info
data TXData =
  TXData
  {
    txSigners :: Signers,
    thisTXID :: TransactionID
  }

-- | The actual contract monad builds on 'FaeRW' by adding escrows and
-- continuation support.
type FaeContractT s m = Coroutine s (StateT Escrows (FaeRWT m))
-- | The commonly used variant
type FaeContract s = FaeContractT s Identity
-- | A "contract transformer", for use with the 'MonadContract' and
-- 'MonadTX' typeclasses.
type ContractT m argType valType = argType -> m (WithEscrows valType)

-- | An internal representation of a contract, partway through full
-- "compilation".  An 'InternalT' has its escrows hidden, because it will
-- need to be evaluated inside a /different/ 'FaeContract' monad that has
-- its own escrows that shouldn't interfere.
type InternalT s argType valType = 
  ContractT (Coroutine s FaeRW) argType valType

-- | Similar to a 'Contract', but accepts an escrow-endowed value and
-- doesn't expose its escrows.
type InternalContract argType valType = 
  InternalT (FaeRequest argType valType) (WithEscrows argType) valType

-- | A contract is, abstractly, a function that can be called in any Fae
-- context, producing a value and a continuation.
newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContract s (Maybe (ConcreteContract argType valType), valType)
  )

-- | The type actually stored in the escrows map, because escrows can have
-- any types of input and output.  Abstract escrows take arbitrary argument
-- types.
type AbstractEscrow = ConcreteContract Dynamic Dynamic
-- | The type actually stored in a 'TransactionEntry', because contracts
-- can have any types of input and output.  Abstract contracts only take
-- string inputs, which they have to parse into the argument type they
-- expect.  This prevents malicious users from injecting their own code
-- into transactions' contract calls.
type AbstractContract = ConcreteContract (String, VersionMap) (Dynamic, VersionMap)

-- | Exception type
data ContractException =
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID |
  MissingSigner String
  deriving (Typeable, Show)

-- * Template Haskell

makeLenses ''Escrows
makeLenses ''TXData

{- Instances -}

-- | Of course
instance Exception ContractException

-- * Functions

-- | Processes a 'Contract' so that it can take escrows on the first call,
-- and hides the escrow storage.
makeInternalT ::
  (Functor s, Functor s') =>
  [BearsValue] ->
  ContractT (FaeContract s') argType valType ->
  FaeContract s (InternalT s' (WithEscrows argType) valType)
makeInternalT eIDs f = do
  Escrows{..} <- takeEscrows eIDs
  txID <- view _thisTXID
  -- It is less crucial that we start the ID chain at a place that reflects
  -- the transaction, but this is nicely uniform with 'useEscrow'.
  let seedID = digest (nextID, txID)
  _nextID %= digest
  entID <- use _nextID
  return $ \(WithEscrows inputEscrows x) ->
    mapMonad (flip evalStateT Escrows{nextID = seedID, ..}) $ do
      _escrowMap %= Map.union inputEscrows
      f x

-- | Takes care of running the contract coroutine and extracting the
-- continuation.  It also correctly retrieves escrows from the argument and
-- places the escrows of the return value into the local storage.
makeConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete f = ConcreteContract $ \x -> do
  xE <- internalSpend x
  result <- lift $ lift $ resume $ f xE -- ugh, double 'lift'
  let 
    (gM, WithEscrows outputEscrows z) =
      case result of
        Left (Request y g) -> (Just g, y)
        Right y -> (Nothing, y)
  _escrowMap %= Map.union outputEscrows
  let gConcM = makeConcrete <$> gM
  return (gConcM, z)

-- | An abstract escrow just needs to check the type of its input and clear
-- the type of its output.
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

-- | An abstract contract needs to parse its input, clear the
-- output type, and also resolve version IDs in the input and save new
-- versions in the output.
makeContractAbstract ::
  forall argType valType.
  (
    Read argType, 
    Versionable argType,
    Versionable valType,
    Typeable argType, Typeable valType
  ) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract f) = ConcreteContract $ \(argS, vers) -> do
  let 
    x = maybe 
      (throw $ BadInputParse argS $ typeRep $ Proxy @argType) 
      (mapVersions vers) 
      (readMaybe argS)
  (gM, y) <- f x
  escrowMap <- use _escrowMap
  let 
    g entID = 
      snd $ fromMaybe (throw $ BadEscrowID entID) $ Map.lookup entID escrowMap
    vMap = versionMap g y
  return (makeContractAbstract <$> gM, (toDyn y, vMap))

-- | Because when we call an escrow, we actually know exactly what type was
-- given as its argument and expected as its result.
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

-- | Retrieves backing escrows from value-bearing objects.
takeEscrows :: (MonadState Escrows m) => [BearsValue] -> m Escrows
takeEscrows xs = do
  escrowMap <- getEscrowMap xs
  nextID <- use _nextID
  return Escrows{..}

-- | Just concatenates the list of all escrows in each of the objects, then
-- turns it into a map.  Internally, this uses an imiation of the @lens@
-- function 'toList' for 'Traversal's, but since an 'EscrowIDTraversal' is
-- not /exactly/ a 'Traversal', we have to reproduce it.
getEscrowMap :: forall m. (MonadState Escrows m) => [BearsValue] -> m EscrowMap
getEscrowMap xs =
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> do
    let
      f :: EscrowIDMap (Writer [m (EntryID, (AbstractEscrow, VersionID))])
      f eID = tell [takeEscrow eID] >> return eID
    sequence $ execWriter $ traverseEscrowIDs f x

-- | Actually looks up an escrow by ID.  This function actually
-- /takes/ the escrows, not just copies them, because valuable things can't
-- be copied.
takeEscrow :: (MonadState Escrows m) => 
  EscrowID argType valType -> m (EntryID, (AbstractEscrow, VersionID))
takeEscrow eID = do
  let k = entID eID
  xM <- use $ _escrowMap . at k
  case xM of
    Just x -> do
      _escrowMap . at k .= Nothing
      return (k, x)
    Nothing -> throw $ BadEscrowID $ entID eID

-- | Because we need to essentially 'spend' a value internally, and we
-- haven't defined that function yet.
internalSpend :: 
  (HasEscrowIDs valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  Escrows{..} <- takeEscrows [bearer x]
  -- We need to force both the escrows and the value because they may have
  -- nonterminating computations in them, which we want to be suffered by
  -- the originating contract and not by its victims.
  return $ WithEscrows escrowMap x

-- | A summary monad-running function.  You start with no escrows and the
-- next escrow ID is the transaction ID.
runFaeContract :: 
  (Monad m) => 
  TransactionID -> Signers -> FaeContractT Naught m a -> m a
runFaeContract thisTXID@(ShortContractID dig) txSigners =
  flip runReaderT TXData{..} .
  fmap fst . runWriterT .
  flip evalStateT Escrows{escrowMap = Map.empty, nextID = dig} .
  runCoroutine

-- | Boosts the base monad from 'Identity'.
hoistFaeContract :: (Monad m, Functor s) => FaeContract s a -> FaeContractT s m a
hoistFaeContract = mapMonad hoistFaeRWST where
  hoistFaeRWST (StateT f) = StateT $ hoistFaeRWT . f
  hoistFaeRWT (WriterT p) = WriterT $ hoistFaeReaderT p
  hoistFaeReaderT (ReaderT f) = reader $ runIdentity . f

-- | Goes all the way up the rather long monad stack.
liftFaeContract :: (Monad m, Functor s) => m a -> FaeContractT s m a
liftFaeContract = lift . lift . lift . lift


{- |
Module: Blockchain.Fae.Internal.Contract
Description: The core 'Contract' type that underlies Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Everything in Fae is a contract, and this module defines contracts and
their construction.

It may be a little confusing how a user-provided 'Contract' becomes the
'AbstractContract' stored by Fae.  This happens in three steps:

  - A 'Contract' is partially "compiled" to an 'InternalContract', which
  handles escrows in the first call and also makes the entire escrow
  storage private.

  - An 'InternalContract' is further "compiled" to a 'ConcreteContract',
  which handles the suspend/resume mechanism and makes a contract basically
  isomorphic to a function from argument to value types.

  - A 'ConcreteContract' is, finally, made into an 'AbstractContract',
  which handles conversion of its input and output types from and to
  untyped 'String' and 'BearsValue, respectively.  It also resolves version
  IDs to their corresponding correctly-typed values.

Each of these stages takes place in some @FaeContract s@ monad, but it is
important to understand that in fact, there are /three/ different monad
contexts occurring in the life cycle of a contract:

  - The one inside the contract, in which the user-defined code is executed;

  - The one in which the contract is created;

  - The one in which the contract is called.

Information is passed among all of these in a manner controlled by the
three steps shown above.  Escrows from the creating context are passed in
via 'bearer' to the internal context; escrows from the return value (and
for escrows themselves, also the argument) are passed via 'WithEscrows';
and the 'FaeRWT' stratum is shared between the internal context and the
calling context.  The effect of the 'InternalContract' stage is to prevent
escrows from being shared between the internal and calling contexts except
as mediated by 'WithEscrows'.
-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Coroutine hiding (Reader)
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.NFData
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

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
-- | A "contract transformer".  The useful one is 'ContractM', though, in
-- "MonadFae".
type ContractT m argType valType = argType -> m (WithEscrows valType)
-- | This wrapper is necessary because 'Fae' and 'FaeTX' are monads that
-- contract authors can actually use, and so we need to carefully limit the
-- capabilities they are allowed.  It's here so we don't export it from
-- "Fae".
newtype FaeM s a = Fae { getFae :: FaeContract s a }
  deriving (Functor, Applicative, Monad)

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
-- context, producing a value and a continuation.  The return-type monad is
-- a 'FaeContractT' rather than just a 'FaeContract' because contracts
-- (and, potentially, escrows) will be run in the former when it is not the
-- latter.
newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s m. (Functor s, Monad m) => 
      argType -> 
      FaeContractT s m (Maybe (ConcreteContract argType valType), valType)
  )

-- | The type actually stored in the escrows map, because escrows can have
-- any types of input and output.  Abstract escrows take arbitrary argument
-- types.
type AbstractEscrow = ConcreteContract BearsValue BearsValue
-- | The type actually stored in a 'TransactionEntry', because contracts
-- can have any types of input and output.  Abstract contracts only take
-- string inputs, which they have to parse into the argument type they
-- expect.  This prevents malicious users from injecting their own code
-- into transactions' contract calls.
type AbstractContract = 
  ConcreteContract (String, VersionMap') (BearsValue, VersionMap)

-- * Template Haskell

makeLenses ''Escrows
makeLenses ''TXData

{- Instances -}

-- * Functions

-- | Processes a 'Contract' so that it can take escrows on the first call,
-- and hides the escrow storage.
makeInternalT ::
  (Functor s, Functor s', Monad m) =>
  [BearsValue] ->
  ContractT (FaeContract s') argType valType ->
  FaeContractT s m (InternalT s' (WithEscrows argType) valType)
makeInternalT eIDs !f = do
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
  (HasEscrowIDs argType, Typeable argType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeConcrete !f = ConcreteContract $ \x -> hoistFaeContract $ do
  xE <- internalSpend x
  result <- lift $ lift $ resume $ f xE -- ugh, double 'lift'
  let 
    (gM, WithEscrows outputEscrows z) =
      case result of
        Left (Request y !g) -> (Just g, y)
        Right y -> (Nothing, y)
  _escrowMap %= Map.union outputEscrows
  let gConcM = makeConcrete <$> gM
  return (gConcM, z)

-- | An abstract escrow just needs to check the type of its input and clear
-- the type of its output.  The 'AbstractEscrow' result does not force its
-- return value.
makeEscrowAbstract ::
  forall argType valType.
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  ConcreteContract argType valType -> AbstractEscrow 
makeEscrowAbstract (ConcreteContract !f) = ConcreteContract $ \xDyn -> do
  let 
    x = unBear xDyn $
      throw $ BadArgType (typeRep (Proxy @argType)) (bearerType xDyn)
  (gM, y) <- f x
  return (makeEscrowAbstract <$> gM, bearer y)

-- | An abstract contract needs to parse its input, clear the
-- output type, and also resolve version IDs in the input and save new
-- versions in the output.  The 'AbstractContract' result deeply forces its
-- return value.
makeContractAbstract ::
  forall argType valType.
  (
    Read argType, NFData valType,
    Versionable argType, Versionable valType,
    Typeable argType, Typeable valType
  ) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract !f) = ConcreteContract $ \(argS, vers) -> do
  let 
    x = maybe 
      (throw $ BadInputParse argS $ typeRep $ Proxy @argType) 
      (mapVersions vers) 
      (readMaybe argS)
  (gM, y) <- f x
  escrowMap <- use _escrowMap
  let 
    h entID = 
      snd $ fromMaybe (throw $ BadEscrowID entID) $ Map.lookup entID escrowMap
    vMap = versionMap h y
  return (makeContractAbstract <$> gM, (bearer $!! y, vMap))

-- | Because when we call an escrow, we actually know exactly what type was
-- given as its argument and expected as its result.
unmakeAbstractEscrow ::
  forall argType valType.
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) =>
  AbstractEscrow -> ConcreteContract argType valType
unmakeAbstractEscrow (ConcreteContract f) = ConcreteContract $ \x -> do
  (gAbsM, yDyn) <- f $ bearer x
  let 
    y = unBear yDyn $
      throw $ BadValType (typeRep (Proxy @valType)) (bearerType yDyn)
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
  (HasEscrowIDs valType, Typeable valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  Escrows{..} <- takeEscrows [bearer x]
  -- We need to force both the escrows and the value because they may have
  -- nonterminating computations in them, which we want to be suffered by
  -- the originating contract and not by its victims.
  return $ WithEscrows escrowMap x

-- | Because in "Transaction" we need to create a rewards escrow.
internalNewEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    Functor s, Monad m
  ) =>
  [BearsValue] -> 
  ContractT (FaeContract (FaeRequest argType valType)) argType valType -> 
  FaeContractT s m (EscrowID argType valType)
internalNewEscrow eIDs f = do
  entID <- use _nextID
  -- modifies nextID
  cAbs <- makeEscrowAbstract . makeConcrete <$> makeInternalT eIDs f
  -- Initial version is entry ID
  _escrowMap %= Map.insert entID (cAbs, VersionID entID)
  return $ EscrowID entID

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


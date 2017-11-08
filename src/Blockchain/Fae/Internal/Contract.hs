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

-- * Types

-- | How escrows are kept in each contract.  Contracts track their escrow
-- ID sequence separately to prevent coupling between different ones.
data Escrows =
  Escrows
  {
    escrowMap :: EscrowMap,
    nextID :: EntryID
  } deriving (Generic)
-- | Convenience type
type EscrowMap = Map EntryID AbstractEscrow
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via `spend` and `release`.
data WithEscrows a = WithEscrows EscrowMap a
-- | The 'Request' type is backwards for my visualization of contracts as
-- coroutines, and also needs to be augmented with backing escrows.
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

-- | This monad contains everything that relates a contract to its
-- surrounding transaction.
type FaeRW = WriterT [AbstractContract] (Reader PublicKey) 

-- | The actual contract monad builds on 'FaeRW' by adding escrows and
-- continuation support.
type FaeContract s = Coroutine s (StateT Escrows FaeRW)
-- | This wrapper is necessary because 'Fae' and 'FaeTX' are monads that
-- contract authors can actually use, and so we need to carefully limit the
-- capabilities they are allowed.
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

-- | An internal representation of a contract, partway through full
-- "compilation".  An 'InternalT' has its escrows hidden, because it will
-- need to be evaluated inside a _different_ 'FaeContract' monad that has
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
type AbstractContract = ConcreteContract Input Dynamic

-- | Exception type
data ContractException =
  BadInputParse String TypeRep |
  BadArgType TypeRep TypeRep | 
  BadValType TypeRep TypeRep |
  BadEscrowID EntryID
  deriving (Typeable, Show)

-- | A representation of an input contract call, containing a reference
-- object (for resolving escrow locators) and an argument string to be
-- parsed into the correct argument type.
data Input = forall a. (HasEscrowIDs a) => Input a String

-- * Type classes

-- | For reading contract arguments as transaction inputs.  Since it is
-- fully defaulted, no one should ever have to define an instance
-- explicitly.  In fact, we don't even export the class members from
-- 'Blockchain.Fae', because it is important to the internal workings of
-- Fae that 'readInput' behave in this way.
class ReadInput a where
  -- | Like 'read', but with a little extra data.
  readInput :: Input -> a
  -- | The default instance reads the argument string, then resolves escrow
  -- locators in it.
  default readInput :: (Read a, Typeable a) => Input -> a
  readInput (Input x s) =
    case filter (null . snd) $ reads s of
      [(y, "")] -> resolveEscrowLocators x y
      _ -> throw $ BadInputParse s (typeRep $ Proxy @a)

{- Instances -}

-- | Of course
instance Exception ContractException

-- | So that we can force escrows as they are returned from contracts.
-- This is necessary to prevent malicious actors from embedding
-- a non-terminating transaction that a recipient will have to execute.
instance NFData Escrows
-- | Because escrows are contracts
instance NFData (ConcreteContract argType valType) where
  rnf (ConcreteContract !f) = ()

-- | Default instance
instance ReadInput Void
-- | Default instance
instance ReadInput ()
-- | Default instance
instance ReadInput Bool
-- | Default instance
instance ReadInput Char
-- | Default instance
instance ReadInput Int
-- | Default instance
instance ReadInput Integer
-- | Default instance
instance ReadInput Float
-- | Default instance
instance ReadInput Double
-- | Default instance
instance (Read a, Typeable a) => ReadInput [a]
-- | Default instance
instance (Read a, Typeable a) => ReadInput (Maybe a)
-- | Default instance
instance (Read a, Typeable a, Read b, Typeable b) => ReadInput (a, b)
-- | Default instance
instance (Read a, Typeable a, Read b, Typeable b) => ReadInput (Either a b)
-- | Default instance
instance 
  (Typeable argType, Typeable valType) =>
  ReadInput (EscrowID argType valType)

-- * Template Haskell

makeLenses ''Escrows

-- * Functions

-- | The high-level escrow constructor, which goes from what a contract
-- author provides to the internal representation of an escrow.
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

-- | The high-level contract constructor, which goes from what a contract
-- author provides to the internal representation of a contract.
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

-- | Processes a 'Contract' so that it can take escrows on the first call,
-- and hides the escrow storage.
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

-- | Uniform definition.  The non-escrow-endowed input to
-- a 'ConcreteContract' is scanned for escrow IDs before it is passed in.
makeEscrowConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType, NFData argType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeEscrowConcrete = makeConcrete internalSpend

-- | Uniform definition.  A contract argument is never endowed with
-- escrows, because it is a security hole to allow callers to pass their
-- own code to called contracts.  This is what transactional escrows
-- handle.
makeContractConcrete ::
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  InternalContract argType valType ->
  ConcreteContract argType valType
makeContractConcrete = makeConcrete $ return . WithEscrows Map.empty

-- | Takes care of running the contract coroutine and extracting the
-- continuation.  It also correctly retrieves escrows from the argument and
-- places the escrows of the return value into the local storage.
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

-- | An abstract contract just needs to parse its input and clear the
-- output type.
makeContractAbstract ::
  forall argType valType.
  (ReadInput argType, Typeable argType, Typeable valType) =>
  ConcreteContract argType valType -> AbstractContract
makeContractAbstract (ConcreteContract f) = ConcreteContract $ \input -> do
  (gM, y) <- f $ readInput input
  return (makeContractAbstract <$> gM, toDyn y)

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
-- not _exactly_ a 'Traversal', we have to reproduce it.
getEscrowMap :: forall m. (MonadState Escrows m) => [BearsValue] -> m EscrowMap
getEscrowMap xs =
  fmap (Map.fromList . join) $ 
  forM xs $ \(BearsValue x) -> do
    let
      f :: EscrowIDMap (Writer [m (EntryID, AbstractEscrow)])
      f eID@EscrowLocator{} = return eID
      f eID = tell [takeEscrow eID] >> return eID
    sequence $ execWriter $ traverseEscrowIDs f x

-- | Actually looks up an escrow by ID.  If an escrow locator has made it
-- this far, this is where it causes an error.  This function actually
-- _takes_ the escrows, not just copies them, because valuable things can't
-- be copied.
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

-- | Because we need to essentially 'spend' a value internally, and we
-- haven't defined that function yet.
internalSpend :: 
  (HasEscrowIDs valType, MonadState Escrows m, NFData valType) => 
  valType -> m (WithEscrows valType)
internalSpend x = do
  Escrows{..} <- takeEscrows [bearer x]
  -- We need to force both the escrows and the value because they may have
  -- nonterminating computations in them, which we want to be suffered by
  -- the originating contract and not by its victims.
  return $ WithEscrows (force escrowMap) (force x)

-- | Because we need to use escrows internally, specifically in
-- 'runTXEscrows'.
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

-- | Traverses all the escrows in a valuable object, picks out the
-- transactional inputs, and executes them, replacing them with outputs.
runTXEscrows :: forall a s. (HasEscrowIDs a, Functor s) => a -> FaeContract s a
runTXEscrows = traverseEscrowIDs f where
  f :: EscrowIDMap (FaeContract s)
  f (TXEscrowIn eID arg) = TXEscrowOut eID <$> internalUseEscrow eID arg
  f eID = return eID


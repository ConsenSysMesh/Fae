{- |
Module: Blockchain.Fae.Internal.Contract
Description: The core 'Contract' type that underlies Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Everything in Fae is a contract, and this module defines contracts and
their construction.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Contract where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Suspend

import Common.Lens

import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.DeepSeq

import Data.ByteString
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Typeable

import qualified Data.Map as Map

import GHC.Generics

import Text.Read (readMaybe)

-- * Types
-- ** Contract data

-- | Infix synonym for a pair to make the argument of 'useEscrow' clearer.
type a <-| b = (a, b)

-- | Infix function to construct the pair synonym.
(<-|) :: a -> b -> a <-| b
(<-|) = (,)

-- | Another dynamic type, this time including 'Exportable'.  Its
-- constructor is strict, so as not to distinguish between an error in the
-- outer value and an error in the inner value.
data ReturnValue =
  forall a. (HasEscrowIDs a, Exportable a) => ReturnValue !a

-- | Something like a closure for the initial state of a new contract
-- (currently only escrows).  The actual contract function can be
-- reconstructed from this, but a 'NamedContract' itself is 'Serializable'
-- while a 'ContractF' is not.
--
-- The fields are strict to ensure that undefined values are detected in
-- the present contract call, and not unexpectedly, later.
data NamedContract name =
  NamedContract
  {
    contractName :: !name,
    contractNextID :: !Digest,
    endowment :: !EscrowMap
  }
  deriving (Generic)
-- | The union of contract entry types for all possible names.
data AnyNamedContract = 
  forall name. (Typeable name) => AnyNamedContract (NamedContract name)
-- | Each escrow has a continually updated version that we need to track;
-- this holds both for the escrow map of a contract and the escrows
-- accompanying a returned value.
--
-- The fields are strict to ensure that undefined values are detected in
-- the present contract call, and not unexpectedly, later.
data EscrowEntry =
  EscrowEntry
  {
    escrowVersion :: !VersionID,
    escrowNameOrFunction :: !(Either AnyNamedContract AbstractLocalContract)
  }
-- | Convenience type.
type EscrowMap = Map EntryID EscrowEntry
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via 'spend' and 'release'.
--
-- This is actually a 'Monad' isomorphic to @Writer EscrowMap@, but it is
-- more work than it is worth to write the instances for that.
data WithEscrows a = 
  WithEscrows 
  {
    withEscrows :: EscrowMap,
    getWithEscrows :: a
  }
  deriving (Generic)
-- | How escrows are kept in each contract.  Contracts track their escrow
-- ID sequence separately to prevent coupling between different ones.
data Escrows =
  Escrows
  {
    escrowMap :: EscrowMap,
    nextID :: Digest
  }

-- ** Transaction data

-- | The relevant transaction info
data TXData =
  TXData
  {
    thisTXSigners :: Signers,
    localHash :: Digest,
    thisTXID :: TransactionID
  }

-- ** Contract outputs

-- | Stores the actual contract function, the type of its 'ContractName'
-- for exporting purposes, and the "nonce", or number of successful contract
-- calls.
data Output =
  Output
  {
    storedContract :: Maybe StoredContract,
    -- | This is not @Maybe@ because even when the contract is spent, we
    -- need its type for exporting.
    contractNameType :: TypeRep 
  }
  deriving (Generic)

data StoredContract =
  StoredContract
  {
    storedFunction :: AbstractGlobalContract,
    storedVersion :: VersionID
  }
  deriving (Generic)

-- ** Internal contract monads

-- | Monad modifier; several of ours use escrows.
type EscrowsT = StateT Escrows
-- | Also used in "Transaction", so needs a name
type TXDataM = Reader TXData
-- | The internal operational monad for Fae contracts.
type FaeExternalM = WriterT [Output] TXDataM
-- | The authoring monad for Fae contracts (when wrapped in 'Fae')
type FaeContractM argType valType = 
  EscrowsT (SuspendT (WithEscrows argType) (WithEscrows valType) FaeExternalM)
-- | The authoring monad for Fae transactions (when wrapped in 'FaeTX')
type FaeTXM = EscrowsT FaeExternalM

-- ** Internal contract functions

-- | User contract with escrows hidden
type PreContractF argType valType = 
  SuspendPreF (WithEscrows argType) (WithEscrows valType) FaeExternalM
-- | A type-correct contract function; 'WithEscrows' is omitted here
-- because neither of the abstract contracts takes it (though they do use
-- it internally).
newtype ContractF m argType valType = 
  ContractF { getContractF :: SuspendStepF argType valType m }
  deriving (Generic, NFData)

-- ** External contract functions

-- | The form of a contract function intended to be called from within
-- a contract, as well as a precursor to an 'AbstractGlobalContract'.
type AbstractLocalContract = ContractF FaeTXM BearsValue BearsValue
-- | The form of a contract function intended to be called from
-- a transaction.  Note that it returns into a 'TXDataM', i.e. does not
-- expose the mutable inner state of the contract, which is revealed
-- read-only in the return value.
type AbstractGlobalContract = 
  ContractF TXDataM String (ReturnValue, EscrowMap, [Output])

-- ** User-visible

-- | The monad that users actually write in, a newtype because we need to
-- limit the capabilities users are allowed to access.
newtype Fae argType valType a = 
  Fae 
  {
    getFae :: FaeContractM argType valType a
  }
  deriving (Functor, Applicative, Monad)
-- | Monad for writing transactions (no continuation), a newtype because we
-- need to limit the capabilities users are allowed to access.
newtype FaeTX a = 
  FaeTX 
  {
    getFaeTX :: FaeTXM a
  }
  deriving (Functor, Applicative, Monad)

-- | The user-provided form of a contract function
type Contract argType valType = ContractT (Fae argType valType) argType valType
-- | Useful generalization to add effects
type ContractT m argType valType = argType -> m (WithEscrows valType)

-- * Fae typeclasses

-- | Instances of this class can be serialized, at least with the
-- assistance of some Fae contextual data (namely, the escrow storage).
class (Typeable a) => Exportable a where
  exportValue :: (MonadState EscrowMap m) => a -> m ByteString
  importValue :: (MonadState EscrowMap m) => ByteString -> m (Maybe a)

-- | Instances of 'ContractName' are always defined by contract authors,
-- who will inevitably have to define 'theContract' to point to a global
-- function.  Since it's global, it is still present when we deserialize,
-- so this is effectively a contract that is portable between Fae
-- instances.
class 
  (HasEscrowIDs a, HasEscrowIDs (ArgType a), HasEscrowIDs (ValType a)) =>
  ContractName a where

  type ArgType a
  type ValType a
  theContract :: a -> Contract (ArgType a) (ValType a)

-- |
-- Instances of this class have access to the full Fae API, allowing them
-- to define multi-stage contracts.  As for 'MonadTX', these instances must
-- have their own evaluation function to get down to the base 'Fae' monad.
-- Notably, 'Transaction's are /not/ written in a 'MonadContract', because
-- they are one-shot.
class (MonadTX m) => MonadContract argType valType m | m -> argType valType where
  -- | Injects the Fae contract API into 'm'.
  liftContract :: Fae argType valType a -> m a

-- |
-- Instances of this class may use the bulk of the Fae API; namely, they
-- may hold value via escrows and create new contracts.  Such monads are
-- intended to be defined by a transformer stack on top of 'FaeTX', hence
-- 'liftTX'; to be useful, they must have a function that "evaluates" them
-- down to 'Fae' or 'FaeTX', so they may define new transactions or
-- contracts.
class (Monad m) => MonadTX m where
  -- | Injects the Fae transaction API into 'm'.
  liftTX :: FaeTX a -> m a

-- * Template Haskell

makeLenses ''WithEscrows
makeLenses ''Escrows
makeLenses ''TXData
makeLenses ''Output
makeLenses ''StoredContract

{- Instances -}

instance NFData Output

instance NFData StoredContract

instance HasEscrowIDs ReturnValue where
  traverseEscrowIDs f (ReturnValue x) = ReturnValue <$> traverseEscrowIDs f x

instance (HasEscrowIDs a, HasEscrowIDs b) => ContractName (Contract a b) where
  type ArgType (Contract a b) = a
  type ValType (Contract a b) = b
  theContract = id

-- | -
instance MonadTX FaeTX where
  liftTX = id

-- | -
instance MonadTX FaeTXM where
  liftTX = getFaeTX

-- | -
instance MonadTX (Fae argType valType) where
  liftTX = Fae . mapStateT lift . getFaeTX

-- | -
instance MonadContract argType valType (Fae argType valType) where
  liftContract = id

-- | An @UndecidableInstance@ for sure
instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadContract argType valType m, Monad (t m)) =>
  MonadContract argType valType (t m) where

  liftContract = lift . liftContract

-- | An @UndecidableInstance@ for sure
instance {-# OVERLAPPABLE #-}
  (MonadTrans t, MonadTX m, Monad (t m)) => 
  MonadTX (t m) where

  liftTX = lift . liftTX

-- * API Functions

-- | Looks up a named signatory, maybe. 
lookupSigner :: (MonadTX m) => String -> m (Maybe PublicKey)
lookupSigner s = liftTX $ FaeTX $ view $ _thisTXSigners . _getSigners . at s

-- | Looks up a named signatory, or throws if not found.
signer :: (MonadTX m) => String -> m PublicKey
signer s = fromMaybe (throw $ MissingSigner s) <$> lookupSigner s

-- | Returns the map of all signatories.
signers :: (MonadTX m) => m (Map String PublicKey)
signers = liftTX $ FaeTX $ view $ _thisTXSigners . _getSigners

-- | Terminates the contract entirely, transferring escrows backing the
-- return value.
spend :: 
  forall m argType valType.
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m (WithEscrows valType)
spend = liftContract . Fae . (takeEscrows >=> lift . terminate)

-- | Terminates the current contract call, transferring escrows backing the
-- return value to the caller and awaiting an argument, depositing its
-- escrows.
release :: 
  forall m argType valType.
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release = liftContract . Fae . (takeEscrows >=> lift . suspend >=> putEscrows)

-- | Emits a new output contract endowed with a given list of valuables.
newContract :: 
  forall m name.
  (
    MonadTX m,
    ContractName name, Read (ArgType name), Exportable (ValType name)
  ) => 
  name -> m ()
newContract x = liftTX $ FaeTX $ do
  (storedVersion, nextID) <- forkNextID
  xE <- takeEscrows x
  let storedFunction = 
        globalContract $ hideEscrows (withEscrows xE) nextID (theContract x)
      contractNameType = typeRep $ Proxy @name
      storedContract = Just StoredContract{..}
  tell [Output{..}]

-- | Creates a new escrow endowed with a given list of valuables.
newEscrow :: 
  (MonadTX m, ContractName name) =>
  name -> m (EscrowID name)
newEscrow contractName = liftTX $ FaeTX $ do
  (entID, contractNextID) <- forkNextID
  WithEscrows endowment _ <- takeEscrows contractName
  let escrowNameOrFunction = Left $ AnyNamedContract NamedContract{..}
      escrowVersion = entID
  _escrowMap %= Map.insert entID EscrowEntry{..}
  return $ EscrowID entID

-- | Calls an escrow by ID, which must exist in the present context.
useEscrow :: 
  (MonadTX m, ContractName name) =>
  [(String, String)] -> EscrowID name -> ArgType name -> m (ValType name)
useEscrow rolePairs eID x = liftTX . FaeTX . joinEscrowState . useNamedEscrow eID $
  \entID escrowVersion nameOrFunction -> return $ do
    let makeLocalCF NamedContract{..} = localContract $ 
          hideEscrows endowment contractNextID (theContract contractName)
        localCF = either makeLocalCF id nameOrFunction
    ~(y, resultCFM) <- remapSigners renames $ typeify (callContract localCF) x
    hash <- view _localHash
    -- The "local hash" corresponds either to the transaction ID (if not in
    -- a contract call) or to the arguments of the contract call, and so
    -- the version will accurately reflect the call history of the escrow.
    let newVer = digest (escrowVersion, hash)
    _escrowMap . at entID .= fmap (EscrowEntry newVer . Right) resultCFM
    return y
  where typeify f = fmap (_1 %~ returnTyped) . f . acceptTyped
        renames = Map.fromList rolePairs

-- * Internal functions

-- ** Calling

-- | Temporarily changes the map of signers according to its first argument.
remapSigners :: (MonadReader TXData m) => Map String String -> m a -> m a
remapSigners renames = local (_thisTXSigners . _getSigners %~ applyRenames) where
  applyRenames keyMap = fmap (flip renameKey keyMap) renames `Map.union` keyMap
  renameKey oldName = Map.findWithDefault (throw $ MissingSigner oldName) oldName

pushArg :: (MonadReader TXData m) => String -> m a -> m a
pushArg s = local (_localHash .~ digest s)

txData :: TransactionID -> Signers -> TXData
txData txID txSigners =
  TXData
  {
    thisTXSigners = txSigners,
    localHash = txID,
    thisTXID = txID
  }

-- | Converts a deeply wrapped function returning an awkward type into
-- a natural stepwise function call.
callContract :: 
  (Monad m) =>
  ContractF m argType valType -> 
  argType -> m (valType, Maybe (ContractF m argType valType))
callContract (ContractF (SuspendStepF f)) = fmap (fmap (fmap ContractF)) . f

-- ** Contract function converters

-- | Generalizes an operationally-correct explicitly-typed contract into an
-- abstract one suitable for calling within other contract code.
localContract :: 
  (HasEscrowIDs argType, HasEscrowIDs valType) =>
  PreContractF argType valType -> AbstractLocalContract
localContract = ContractF . 
  alterSuspendStepF acceptLocal returnLocal lift . startSuspendF

-- | Generalizes an operationally-correct explicitly-typed contract into an
-- abstract one suitable for being called as a transaction input.
globalContract :: 
  (
    Read argType, Exportable valType,
    HasEscrowIDs argType, HasEscrowIDs valType
  ) =>
  PreContractF argType valType -> AbstractGlobalContract
globalContract = ContractF . mapSuspendStepF extractState .
  alterSuspendStepF acceptGlobal returnGlobal lift . startSuspendF
  where
    extractState xm = do
      ((y, sfM), s, w) <- unFaeTXM xm
      return ((y, escrowMap s, w), sfM)
    unFaeTXM xm = do
      txID <- view _thisTXID
      ~(~(x, s), w) <- runWriterT $ runStateT xm (Escrows Map.empty txID)
      return (x, s, w)

-- | Prepares a value-bearing argument.
acceptLocal :: 
  forall argType.
  (HasEscrowIDs argType) =>
  BearsValue -> FaeTXM (WithEscrows argType)
acceptLocal xDyn = takeEscrows x where
  x = unBear xDyn $ 
        throw $ BadArgType (typeRep $ Proxy @argType) (bearerType xDyn)

-- | Prepares a value-bearing result.
returnLocal :: 
  (HasEscrowIDs valType) =>
  WithEscrows valType -> FaeTXM BearsValue
returnLocal = fmap bearer . putEscrows

-- | Prepares a literal argument together with a lookup table of versioned
-- values for its version references.
acceptGlobal :: 
  forall argType.
  (Read argType, HasEscrowIDs argType) =>
  String -> FaeTXM (WithEscrows argType)
-- Laziness assurance: the 'fromMaybe' function (which is not lazy) is
-- nonetheless safe here because 'argS' is not provided by user code, and
-- 'readMaybe' always returns a good value.
acceptGlobal argS = takeEscrows $ fromMaybe err $ readMaybe argS where
  err = throw $ BadInputParse argS $ typeRep $ Proxy @argType

-- | Prepares a value-bearing result together with its table of versioned
-- values.  N.B. The 'ReturnValue' component is _always_ defined, though
-- its inner value may not be.
returnGlobal :: 
  (HasEscrowIDs valType, Exportable valType) =>
  WithEscrows valType -> FaeTXM ReturnValue
returnGlobal = fmap ReturnValue . putEscrows

-- | Prepares a typed value to be passed to an abstract function.
acceptTyped :: (HasEscrowIDs argType) => argType -> BearsValue
acceptTyped = bearer

-- | Prepares an abstract function's return value as typed.
returnTyped :: forall valType. (HasEscrowIDs valType) => BearsValue -> valType
returnTyped yDyn = unBear yDyn $
  throw $ BadValType (typeRep $ Proxy @valType) (bearerType yDyn)

-- ** Escrow manipulation

-- | Sets up the contract function to accept an escrow-backed initial
-- argument, then initializes its storage, removing it from scope.
hideEscrows :: 
  EscrowMap -> Digest -> Contract argType valType -> PreContractF argType valType
hideEscrows escrowMap nextID f = 
  \xE -> evalStateT (putEscrows xE >>= getFae . f) Escrows{..} 

-- | Computes the current next ID and the one that starts a new "branch",
-- and bumps the next ID in the current "branch".
forkNextID :: FaeTXM (Digest, Digest)
forkNextID = do
  oldNextID <- use _nextID
  hash <- view _localHash
  -- It is less crucial that we start the ID chain at a place that reflects
  -- the transaction, but this is nicely uniform with 'useEscrow'.
  _nextID %= digest
  let forkedNextID = digest (oldNextID, hash)
  return (oldNextID, forkedNextID)

-- | Places the escrows backing a value into storage.
putEscrows :: (MonadState Escrows m) => WithEscrows a -> m a
putEscrows ~(WithEscrows escrows a) = do
  _escrowMap %= Map.union escrows
  return a

-- | Extracts the escrows backing a single value.
takeEscrows :: 
  (HasEscrowIDs valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
takeEscrows x = do
  escrowMap <- liftEscrowState $ getEntIDMap (takeEntry . entID) x
  return $ WithEscrows escrowMap x

-- | Gets an escrow entry, casting the contract name alternative to its
-- correct type, and applies a well-typed function.
useNamedEscrow :: 
  (MonadState EscrowMap m, Typeable name) => 
  EscrowID name -> 
  (
    EntryID -> 
    VersionID -> 
    Either (NamedContract name) AbstractLocalContract -> 
    m a
  ) ->
  m a
useNamedEscrow eID@EscrowID{..} f = checkApply =<< getEntry entID where
  checkApply x@EscrowEntry{..} = f entID escrowVersion nameOrFunction where
    nameOrFunction = bimap checkedCast id escrowNameOrFunction
    checkedCast (AnyNamedContract c) =
      fromMaybe (throw $ BadEscrowName entID (typeRep eID) (typeRep c)) $ cast c

-- | Upgrades a computation using only the escrow map itself to one using
-- the whole 'Escrows' object.
liftEscrowState :: (MonadState Escrows m) => State EscrowMap a -> m a
liftEscrowState xm = do
  (y, newEscrowMap) <- runState xm <$> use _escrowMap
  _escrowMap .= newEscrowMap
  return y

-- | Helpful for encapsulating computations in 'useNamedEscrow' that
-- require a full @MonadState Escrows@.
joinEscrowState :: (MonadState Escrows m) => State EscrowMap (m a) -> m a
joinEscrowState = join . liftEscrowState

-- | Gets the escrow version from a specific escrow storage map.
lookupWithEscrows :: EscrowMap -> EntryID -> VersionID
lookupWithEscrows escrowMap entID =
  maybe (throw $ BadEscrowID entID) escrowVersion $ Map.lookup entID escrowMap

-- ** 'ReturnValue' manipulation

-- | Like 'unBear'.
getReturnValue :: (Typeable a) => ReturnValue -> Maybe a
getReturnValue (ReturnValue x) = cast x

-- | Like 'bearerType'.
returnValueType :: ReturnValue -> TypeRep
returnValueType (ReturnValue x) = typeRep (Just x)

-- | Taking advantage of the existential type
exportReturnValue :: (MonadState EscrowMap m) => ReturnValue -> m ByteString
exportReturnValue (ReturnValue x) = exportValue x


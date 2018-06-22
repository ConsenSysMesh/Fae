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
import Blockchain.Fae.Internal.Versions

import Common.Lens

import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Void

import qualified Data.Map as Map

import GHC.Generics

import Text.Read (readMaybe)

import Type.Reflection

-- * Types
-- ** Contract data

data ReturnValue =
  forall a. (Versionable a, HasEscrowIDs a, Exportable a) => ReturnValue a

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
    endowment :: !EscrowMap
  }
  deriving (Generic)
-- | The union of contract entry types for all possible names.
data AnyNamedContract = 
  forall name. (ContractName name) => AnyNamedContract (NamedContract name)
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
data WithEscrows a = WithEscrows EscrowMap a deriving (Generic)
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
    thisTXID :: TransactionID
  }

-- ** Internal contract monads

-- | Useful alias for a rather long term.
type OutputsList = [(SomeTypeRep, AbstractGlobalContract)]
-- | Monad modifier; several of ours use escrows.
type EscrowsT = StateT Escrows
-- | The internal operational monad for Fae contracts.
type FaeExternalM = ReaderT TXData (Writer OutputsList)
-- | The authoring monad for Fae contracts (when wrapped in 'FaeM')
type FaeContractM argType valType = 
  EscrowsT (SuspendT (WithEscrows argType) (WithEscrows valType) FaeExternalM)
-- | The authoring monad for Fae transactions (when wrapped in 'FaeM')
type FaeTXM = EscrowsT FaeExternalM

-- ** Internal contract functions

-- | User contract with escrows hidden
type PreContractF argType valType = 
  SuspendPreF (WithEscrows argType) (WithEscrows valType) FaeExternalM
-- | A type-correct contract function; 'WithEscrows' is omitted here
-- because neither of the abstract contracts takes it (though they do use
-- it internally).
newtype ContractF argType valType = 
  ContractF { getContractF :: SuspendStepF argType valType FaeTXM }
  deriving (Generic, NFData)

-- ** External contract functions

-- | The form of a contract function intended to be called from within
-- a contract, as well as a precursor to an 'AbstractGlobalContract'.
type AbstractLocalContract = ContractF BearsValue BearsValue
-- | The form of a contract function intended to be called from
-- a transaction.
type AbstractGlobalContract = 
  ContractF (String, VersionMap') (ReturnValue, VersionMap)

-- ** User-visible

-- | This wrapper is necessary because 'Fae' and 'FaeTX' are monads that
-- contract authors can actually use, and so we need to carefully limit the
-- capabilities they are allowed.
newtype FaeM m a = Fae { getFae :: m a }
  deriving (Functor, Applicative, Monad)

-- | The monad that users actually write in
type Fae argType valType = FaeM (FaeContractM argType valType)
-- | Monad for writing transactions (no continuation)
type FaeTX = FaeM FaeTXM

-- | The user-provided form of a contract function
type Contract argType valType = ContractT (Fae argType valType) argType valType
-- | Useful generalization to add effects
type ContractT m argType valType = argType -> m (WithEscrows valType)
-- | The user-provided form of a transaction function.  Despite the similar
-- form, the meanings of 'a' and 'b' here are rather different than
-- 'argType' and 'valType' for a 'Contract', because transactions accept
-- a data type constructed from the return values of various contract
-- calls, and return without preserving value.
type Transaction a b = a -> FaeTX b

-- * Fae typeclasses

-- | Instances of this class can be serialized, at least with the
-- assistance of some Fae contextual data (namely, the escrow storage).
class Exportable a where
  exportValue :: (MonadState Escrows m) => a -> m ByteString
  importValue :: (MonadState Escrows m) => ByteString -> m (Maybe a)

-- | Instances of 'ContractName' are always defined by contract authors,
-- who will inevitably have to define 'theContract' to point to a global
-- function.  Since it's global, it is still present when we deserialize,
-- so this is effectively a contract that is portable between Fae
-- instances.
class 
  (
    Typeable a,
    Versionable (ArgType a), Versionable (ValType a),
    HasEscrowIDs (ArgType a), HasEscrowIDs (ValType a)
  ) => ContractName a where

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

makeLenses ''Escrows
makeLenses ''TXData

{- Instances -}

-- | -
instance MonadTX FaeTX where
  liftTX = id

-- | -
instance MonadTX (Fae argType valType) where
  liftTX = Fae . mapStateT lift . getFae

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
lookupSigner s = liftTX $ Fae $ view $ _thisTXSigners . _getSigners . at s

-- | Looks up a named signatory, or throws if not found.
signer :: (MonadTX m) => String -> m PublicKey
signer s = fromMaybe (throw $ MissingSigner s) <$> lookupSigner s

-- | Returns the map of all signatories.
signers :: (MonadTX m) => m (Map String PublicKey)
signers = liftTX $ Fae $ view $ _thisTXSigners . _getSigners

-- | Terminates the contract entirely, transferring escrows backing the
-- return value.
spend :: 
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m (WithEscrows valType)
spend = liftContract . Fae . (takeEscrows >=> lift . terminate)

-- | Terminates the current contract call, transferring escrows backing the
-- return value to the caller and awaiting an argument, depositing its
-- escrows.
release :: 
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release = liftContract . Fae . (takeEscrows >=> lift . suspend >=> putEscrows)

-- | Emits a new output contract endowed with a given list of valuables.
newContract :: 
  forall name m.
  (
    ContractName name, Read (ArgType name), Exportable (ValType name), 
    MonadTX m
  ) => 
  [BearsValue] -> name -> m ()
newContract values x = liftTX $ Fae $ do
  escrowMap <- getEscrowMap values
  contractF <- globalContract <$> hideEscrows escrowMap (theContract x)
  tell [(someTypeRep $ Proxy @name, contractF)]

-- | Creates a new escrow endowed with a given list of valuables.
newEscrow :: 
  (ContractName name, MonadTX m) =>
  [BearsValue] -> name -> m (EscrowID name)
newEscrow values contractName = liftTX $ Fae $ do
  entID <- use _nextID
  endowment <- getEscrowMap values
  let escrowNameOrFunction = Left $ AnyNamedContract NamedContract{..}
      escrowVersion = VersionID entID
  _escrowMap %= Map.insert entID EscrowEntry{..}
  return $ EscrowID entID

-- | Calls an escrow by ID, which must exist in the present context.
useEscrow :: 
  (ContractName name, MonadTX m) =>
  EscrowID name -> ArgType name -> m (ValType name)
useEscrow EscrowID{..} x = liftTX $ Fae $ do
  EscrowEntry{..} <-
    use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  let makeLocalCF (AnyNamedContract NamedContract{..}) = 
        localContract <$> hideEscrows endowment (theContract contractName)
  localCF <- either makeLocalCF return escrowNameOrFunction
  ~(y, resultCFM) <- typeify (callContract localCF) x
  txID <- view _thisTXID
  -- We hash with the transaction ID so that each new version reflects how
  -- it was created.  If the transaction is a known quantity, then this
  -- ensures that the version accurately reflects its effects and not those
  -- of some other, hidden, transaction.
  let newVer = mkVersionID (escrowVersion, txID)
  _escrowMap . at entID .= fmap (EscrowEntry newVer . Right) resultCFM
  return y

  where typeify f = fmap (_1 %~ returnTyped) . f . acceptTyped

-- * Internal functions

-- ** Contract function converters

-- | Converts a deeply wrapped function returning an awkward type into
-- a natural stepwise function call.
callContract :: 
  ContractF argType valType -> 
  argType -> FaeTXM (valType, Maybe (ContractF argType valType))
callContract (ContractF (SuspendStepF f)) = fmap (fmap (fmap ContractF)) . f

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
    Versionable argType, Versionable valType,
    HasEscrowIDs argType, HasEscrowIDs valType
  ) =>
  PreContractF argType valType -> AbstractGlobalContract
globalContract = ContractF . 
  alterSuspendStepF acceptGlobal returnGlobal lift . startSuspendF

-- | Prepares a value-bearing argument.
acceptLocal :: 
  forall argType.
  (HasEscrowIDs argType) =>
  BearsValue -> FaeTXM (WithEscrows argType)
acceptLocal xDyn = takeEscrows x where
  x = unBear xDyn $ 
        throw $ BadArgType (someTypeRep $ Proxy @argType) (bearerType xDyn)

-- | Prepares a value-bearing result.
returnLocal :: 
  (HasEscrowIDs valType) =>
  WithEscrows valType -> FaeTXM BearsValue
returnLocal = fmap bearer . putEscrows

-- | Prepares a literal argument together with a lookup table of versioned
-- values for its version references.
acceptGlobal :: 
  forall argType.
  (Read argType, Versionable argType, HasEscrowIDs argType) =>
  (String, VersionMap') -> FaeTXM (WithEscrows argType)
acceptGlobal (argS, vers) = takeEscrows x where
  -- Laziness assurance: the 'maybe' function (which is not lazy) is
  -- nonetheless safe here because 'argS' is not provided by user code, and
  -- 'readMaybe' always returns a good value.
  x = maybe 
    (throw $ BadInputParse argS $ someTypeRep $ Proxy @argType) 
    (mapVersions vers) 
    (readMaybe argS)

-- | Prepares a value-bearing result together with its table of versioned
-- values.
returnGlobal :: 
  (HasEscrowIDs valType, Versionable valType, Exportable valType) =>
  WithEscrows valType -> FaeTXM (ReturnValue, VersionMap)
returnGlobal yE = do
  y <- putEscrows yE
  escrowMap <- use _escrowMap
  return (ReturnValue y, versionMap (lookupWithEscrows escrowMap) y)

-- | Prepares a typed value to be passed to an abstract function.
acceptTyped :: (HasEscrowIDs argType) => argType -> BearsValue
acceptTyped = bearer

-- | Prepares an abstract function's return value as typed.
returnTyped :: forall valType. (HasEscrowIDs valType) => BearsValue -> valType
returnTyped yDyn = unBear yDyn $
  throw $ BadValType (someTypeRep $ Proxy @valType) (bearerType yDyn)

-- ** Escrow manipulation

-- | Sets up the contract function to accept an escrow-backed initial
-- argument, then initializes its storage, removing it from scope.
hideEscrows :: 
  EscrowMap -> Contract argType valType -> 
  FaeTXM (PreContractF argType valType)
hideEscrows escrowMap f = do
  oldNextID <- use _nextID
  txID <- view _thisTXID
  -- It is less crucial that we start the ID chain at a place that reflects
  -- the transaction, but this is nicely uniform with 'useEscrow'.
  let nextID = digest (oldNextID, txID)
  _nextID %= digest
  return $ \xE -> evalStateT (putEscrows xE >>= getFae . f) Escrows{..} 

-- | Places the escrows backing a value into storage.
putEscrows :: (MonadState Escrows m) => WithEscrows a -> m a
putEscrows ~(WithEscrows escrows a) = do
  _escrowMap %= Map.union escrows
  return a

-- | Extracts the escrows backing a single value.
takeEscrows :: 
  (HasEscrowIDs valType, MonadState Escrows m) => 
  valType -> m (WithEscrows valType)
takeEscrows y = do
  escrowMap <- getEscrowMap y
  return $ WithEscrows escrowMap y

-- | Just concatenates the list of all escrows in each of the objects, then
-- turns it into a map.  Internally, this uses an imitation of the @lens@
-- function 'toList' for 'Traversal's, but since an 'EscrowIDTraversal' is
-- not /exactly/ a 'Traversal', we have to reproduce it.
getEscrowMap :: 
  forall m a.
  (MonadState Escrows m, HasEscrowIDs a) => a -> m EscrowMap
getEscrowMap = fmap Map.fromList . sequence . execWriter . traverseEscrowIDs f 
  where 
    f :: EscrowIDMap (Writer [m (EntryID, EscrowEntry)]) 
    f eID = tell [takeEscrow eID] >> return eID

-- | This function actually /takes/ the escrows, not just copies them,
-- because valuable things can't be copied.
takeEscrow :: 
  (MonadState Escrows m, Typeable name) => 
  EscrowID name -> m (EntryID, EscrowEntry)
takeEscrow eID = do
  x <- peekEscrow eID
  _escrowMap . at k .= Nothing
  return (k, x)
  where k = entID eID

-- | Actually looks up an escrow by ID, checking the contract name if the
-- escrow is still in its initial state where the name is known.
peekEscrow :: 
  forall m name.
  (MonadState Escrows m, Typeable name) => 
  EscrowID name -> m EscrowEntry
peekEscrow (EscrowID entID) = do
  x@EscrowEntry{..} <- 
    use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  return $ case escrowNameOrFunction of
    Left c -> nameContract @name entID c `seq` x
    _ -> x

lookupWithEscrows :: EscrowMap -> EntryID -> VersionID
lookupWithEscrows escrowMap entID =
  maybe (throw $ BadEscrowID entID) escrowVersion $ Map.lookup entID escrowMap

-- * 'ContractName' manipulation

-- | Extracts the true type, if that is what is expected.
nameContract :: 
  forall name. (Typeable name) => EntryID -> AnyNamedContract -> NamedContract name
nameContract entID (AnyNamedContract c)
  | Just HRefl <- typeOf c `eqTypeRep` typeRep @(NamedContract name) = c
  | otherwise = 
      throw $ BadEscrowName entID (someTypeRep $ Proxy @name) (contractNameRep c)

-- | Just for neatness; gets the contract name of an escrow.
contractNameRep :: 
  forall name. (Typeable name) => NamedContract name -> SomeTypeRep
contractNameRep _ = someTypeRep $ Proxy @name

-- ** 'ReturnValue' manipulation

-- | Like 'unBear'.
getReturnValue :: (Typeable a) => ReturnValue -> a -> a
getReturnValue (ReturnValue x) x0 
  | Just HRefl <- typeOf x `eqTypeRep` typeOf x0 = x
  | otherwise = x0

-- | Like 'bearerType'.
returnValueType :: ReturnValue -> SomeTypeRep
returnValueType (ReturnValue x) = someTypeRep (Just x)

-- | Taking advantage of the existential type
exportReturnValue :: ReturnValue -> FaeTXM ByteString
exportReturnValue (ReturnValue x) = exportValue x

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
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Suspend
import Blockchain.Fae.Internal.Versions

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.DeepSeq

import Data.Bifunctor
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
-- ** Contract data

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
type EscrowMap = Map EntryID (AbstractLocalContract, VersionID)
-- | This type encodes a value together with its backing escrows.  It is
-- constructed automatically, and you can't construct it manually, so it is
-- only useful in type signatures; it forces contracts to return values
-- only via 'spend' and 'release'.
data WithEscrows a = WithEscrows EscrowMap a

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
type OutputsList = [AbstractGlobalContract]
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
  deriving (Generic)

-- ** External contract functions

-- | The form of a contract function intended to be called from within
-- a contract, as well as a precursor to an 'AbstractGlobalContract'.
type AbstractLocalContract = ContractF BearsValue BearsValue
-- | The form of a contract function intended to be called from
-- a transaction.
type AbstractGlobalContract = 
  ContractF (String, VersionMap') (BearsValue, VersionMap)

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
instance NFData Escrows

-- | -
instance NFData (ContractF argType valType)

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
spend = liftContract . Fae . 
  (mapStateT lift . protect >=> takeEscrows >=> lift . terminate)

-- | Terminates the current contract call, transferring escrows backing the
-- return value to the caller and awaiting an argument, depositing its
-- escrows.
release :: 
  (HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release = liftContract . Fae . 
  (mapStateT lift . protect >=> takeEscrows >=> lift . suspend >=> putEscrows)

-- | Emits a new output contract endowed with a given list of valuables.
newContract :: 
  (
    Versionable argType, Versionable valType,
    HasEscrowIDs argType, HasEscrowIDs valType,
    Read argType, MonadTX m
  ) => 
  [BearsValue] -> Contract argType valType -> m ()
newContract values f = liftTX $ Fae $ do
  contractF <- globalContract <$> hideEscrows values f
  tell [contractF]

-- | Creates a new escrow endowed with a given list of valuables.
newEscrow :: 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadTX m) =>
  [BearsValue] -> Contract argType valType -> m (EscrowID argType valType)
newEscrow values f = liftTX $ Fae $ do
  escrowF <- localContract <$> hideEscrows values f
  entID <- use _nextID
  _escrowMap %= Map.insert entID (escrowF, VersionID entID)
  return $ EscrowID entID

-- | Calls an escrow by ID, which must exist in the present context.
useEscrow :: 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadTX m) =>
  EscrowID argType valType -> argType -> m valType
useEscrow EscrowID{..} x = liftTX $ Fae $ do
  ~(localCF, ver) <- 
    use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  ~(y, localCFM) <- typeify (callContract localCF) x
  txID <- view _thisTXID
  -- We hash with the transaction ID so that each new version reflects how
  -- it was created.  If the transaction is a known quantity, then this
  -- ensures that the version accurately reflects its effects and not those
  -- of some other, hidden, transaction.
  let newVer = mkVersionID (ver, txID)
  _escrowMap . at entID .= fmap (, newVer) localCFM
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
    Read argType, 
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
  x = unBear xDyn $ throw $ BadArgType (typeRep (Proxy @argType)) (bearerType xDyn)

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
    (throw $ BadInputParse argS $ typeRep $ Proxy @argType) 
    (mapVersions vers) 
    (readMaybe argS)

-- | Prepares a value-bearing result together with its table of versioned
-- values.
returnGlobal :: 
  (HasEscrowIDs valType, Versionable valType) =>
  WithEscrows valType -> FaeTXM (BearsValue, VersionMap)
returnGlobal yE = do
  y <- putEscrows yE
  escrowMap <- use _escrowMap
  let
    vLookup entID = 
      snd $ fromMaybe (throw $ BadEscrowID entID) $ Map.lookup entID escrowMap
  return (bearer y, versionMap vLookup y)

-- | Prepares a typed value to be passed to an abstract function.
acceptTyped :: (HasEscrowIDs argType) => argType -> BearsValue
acceptTyped = bearer

-- | Prepares an abstract function's return value as typed.
returnTyped :: forall valType. (HasEscrowIDs valType) => BearsValue -> valType
returnTyped yDyn = unBear yDyn $
  throw $ BadValType (typeRep (Proxy @valType)) (bearerType yDyn)

-- ** Escrow manipulation

-- | Sets up the contract function to accept an escrow-backed initial
-- argument, then initializes its storage, removing it from scope.
hideEscrows :: 
  [BearsValue] -> Contract argType valType -> 
  FaeTXM (PreContractF argType valType)
hideEscrows values f = do
  oldNextID <- use _nextID
  txID <- view _thisTXID
  -- It is less crucial that we start the ID chain at a place that reflects
  -- the transaction, but this is nicely uniform with 'useEscrow'.
  let nextID = digest (oldNextID, txID)
  _nextID %= digest
  escrowMap <- getEscrowMap values
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
  escrowMap <- getEscrowMap [bearer y]
  return $ WithEscrows escrowMap y

-- | Just concatenates the list of all escrows in each of the objects, then
-- turns it into a map.  Internally, this uses an imitation of the @lens@
-- function 'toList' for 'Traversal's, but since an 'EscrowIDTraversal' is
-- not /exactly/ a 'Traversal', we have to reproduce it.
getEscrowMap :: forall m. (MonadState Escrows m) => [BearsValue] -> m EscrowMap
getEscrowMap xs = do
  -- This redundant construction exploits the laziness of the underlying
  -- monad (which it hopefully has) to keep the final monadic object
  -- defined, with the exceptions buried in the return value.
  result <- fmap (Map.fromList . join) $ mapM (sequence . getTakers) xs
  return result
  where
    getTakers (BearsValue x) = execWriter $ traverseEscrowIDs f x

    f :: EscrowIDMap (Writer [m (EntryID, (AbstractLocalContract, VersionID))])
    f eID = tell [takeEscrow eID] >> return eID

-- | Actually looks up an escrow by ID.  This function actually
-- /takes/ the escrows, not just copies them, because valuable things can't
-- be copied.
takeEscrow :: (MonadState Escrows m) => 
  EscrowID argType valType -> m (EntryID, (AbstractLocalContract, VersionID))
takeEscrow eID = do
  -- These lines are carefully written so that the monad itself is always
  -- defined, and any exceptions are buried in the components of the return
  -- value.
  xM <- use $ _escrowMap . at k
  _escrowMap . at k .= Nothing
  return $ maybe (throw $ BadEscrowID $ entID eID) (k,) xM
  where k = entID eID

-- ** Laziness

-- | So that a contract with a bad escrow map doesn't get saved.  Note that
-- this doesn't detect deep errors, i.e. errors that only arise when an
-- escrow is called.  Caveat emptor!
protect :: valType -> FaeTXM valType
protect y = do
  escrows <- get 
  return $ escrows `deepseq` y


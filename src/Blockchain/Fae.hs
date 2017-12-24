{-# LANGUAGE UndecidableInstances, Trustworthy #-}
{- |
Module: Blockchain.Fae
Description: A functional smart contract system
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae is a functional smart contract system, a "functional alternative to
Ethereum".  This module exposes the contract authoring API.

In Fae, a /contract/ is a function from "valued input" to "valued output".
The value is backed by /escrows/, which are contracts within contracts, and
which are transferred along with the output they back when it is returned
from the containing contract.  While escrows may be called by ID and input
argument anywhere in contract code, a non-escrow contract (one that does
not belong to any other contract) may only be called in the preamble to
a /transaction/.  The reason for this will be explained later.

Contracts are modeled as /coroutines/, or functions that may be suspended
and resumed.  Suspension, in the form of "releasing" a value, causes
contract execution to end, returning the value, and causing the contract to
be updated in storage to refer to the subsequent portion, which is resumed
when the contract is next called.  Contracts are therefore entirely
responsible for the nature of their persistent state, and may indeed be
written in a monad with various state effects (reader, writer, and state
being the obvious ones), or with none at all.

Contract storage is /lazy/, in that no entries are evaluated unless
strictly required.  No activity within Fae strictly requires this
evaluation, because ultimately, all contract executions are reflected in
transaction results or new contracts, which are themselves stored lazily.
It is expected that parties interested in the contents of the storage will
access it in a sandbox, only upon which will its entries finally be
evaluated to the extent necessary.  Therefore, submitting new transactions
to Fae is very cheap, as only a minimal amount of bookkeeping computation
is done.

Unlike Ethereum, Fae has no computational fees ("gas").  The reason for
this is that, because of the lazy design, one is never forced to execute
untrusted code.  This is facilitated by the requirement that non-escrow
contracts only be called in advance of transaction execution; in addition,
the arguments to these contracts are limited to literals or outputs of
trusted other contracts.  Therefore no untrusted code, either in the
surrounding transaction or in the argument, intervenes in contract
execution.

Fae is not a completely pure functional system: it has exceptions.  This is
both because contract code, being Haskell, may throw exceptions for
syntactic reasons, and because the system itself may raise exceptions.
Although contracts may throw exceptions, they may never catch them, lest
they hide an error that shouldn't be recoverable.
-}
module Blockchain.Fae
  (
    -- * Transactions
    -- | Transactions are handled outside of Fae's Haskell API, but their
    -- definitions are still within it.  When processed, a transaction must
    -- be accompanied by a list of @(ContractID, String)@ pairs denoting
    -- the literal arguments passed to the contracts with the given IDs.
    -- These are then 'read' into Haskell types, to prevent malicious
    -- authors from inserting nonterminating code into the contract calls.
    Transaction, TransactionM, TransactionID, PublicKey, FaeTX, MonadTX,
    -- * Contracts and escrows
    Contract, ContractM, ContractID(..), Fae, MonadContract,
    EscrowID, BearsValue, RewardEscrowID, 
    spend, release, useEscrow, newEscrow, newContract,
    lookupSigner, signer, Blockchain.Fae.signers, claimReward, bearer, 
    -- * Versioning
    -- | In order to ensure that transaction authors can rely on getting
    -- the escrow-backed values they expect, contract outputs are
    -- "versioned" and any change to any escrow ID alters the version.
    -- Contract literal arguments can refer to these values by version.
    Versioned(Versioned, getVersioned),
    -- * Opaque classes
    GetInputValues, HasEscrowIDs, Versionable, 
    -- * Re-exports
    Natural, Typeable, Exception, Generic, NFData, Void, throw, evaluate
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Versions

import Control.DeepSeq

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Writer.Class

import Data.Dynamic
import Data.Maybe
import Data.Sequence (Seq)
import Data.Typeable

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Exception (Exception, throw, evaluate)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- * Types

-- | The monad for multi-stage contracts taking an 'argType' and returning
-- a 'valType'.  These may not be transactions.
type Fae argType valType = FaeM (FaeRequest argType valType)
-- | The monad for single-stage contracts, i.e. transactions.
type FaeTX = FaeM Naught
-- | This is the type to use when defining a new contract.  Its argument is
-- the argument given in the first call to this contract; all subsequent
-- calls pass in their arguments via 'release'.
type Contract argType valType = ContractT (Fae argType valType) argType valType
-- | A contract transformer to apply effects to 'Fae'
type ContractM (t :: (* -> *) -> (* -> *)) argType valType =
  ContractT (t (Fae argType valType)) argType valType
-- | A transaction transformer like 'ContractM'
type TransactionM (t :: (* -> *) -> (* -> *)) a b = a -> t FaeTX b

-- * Type classes

-- |
-- Instances of this class have access to the full Fae API, allowing them
-- to define multi-stage contracts.  As for 'MonadTX', these instances must
-- have their own evaluation function to get down to the base 'Fae' monad.
-- Notably, 'Transaction's are /not/ written in a 'MonadContract', because
-- they are one-shot.
class 
  (HasEscrowIDs argType, HasEscrowIDs valType, MonadTX m) => 
  MonadContract argType valType m | m -> argType valType where

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

{- Instances -}

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

-- | Of course
instance 
  (HasEscrowIDs argType, HasEscrowIDs valType) => 
  MonadContract argType valType (Fae argType valType) where

  liftContract = id

-- | A little backwards, since 'FaeContract' lies below rather than above
-- 'FaeTX', but they are actually isomorphic, and we need to start the
-- monad stack down here so that we can derive @forall s. (Functor s) =>
-- MonadTX (FaeM s)@. 
instance (Functor s) => MonadTX (FaeContract s) where
  liftTX (Fae xM) = mapSuspension (const undefined) xM

-- | Of course
deriving instance (Functor s) => MonadTX (FaeM s)

-- * Functions

-- | This function is like 'return' but also ensures that the returned
-- value is passed with its backing escrows, maintaining its value.  Once
-- a contract terminates with a 'spend', it is removed from storage.
spend :: 
  (Typeable valType, HasEscrowIDs valType, MonadContract argType' valType' m) => 
  valType -> m (WithEscrows valType)
spend = liftTX . Fae . internalSpend 

-- | This function is used in an expression like @nextArg <- release
-- currentValue@ to suspend the current contract, simultaneously
-- `spend`ing an intermediate value, and awaiting its next call to
-- continue with the arg that was passed.
release :: 
  (Typeable valType, HasEscrowIDs valType, MonadContract argType valType m) => 
  valType -> m argType
release x = liftContract $ Fae $ do
  req <- internalSpend x
  escrows <- get
  suspend $ Request req $ \(WithEscrows inputEscrows y) -> do
    put escrows -- makeInternalT wipes out the ongoing escrows
    _escrowMap %= Map.union inputEscrows
    return y

-- | Calls the given escrow by ID as a function.  This causes the escrow to
-- be updated, and possibly deleted if it terminated with a `spend`.
useEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    MonadTX m
  ) =>
  EscrowID argType valType -> argType -> m valType
useEscrow EscrowID{..} x = liftTX $ Fae $ do
  (fAbs, ver) <- 
    use $ _escrowMap . at entID . defaultLens (throw $ BadEscrowID entID)
  let ConcreteContract f = unmakeAbstractEscrow fAbs
  (gConcM, y) <- f x
  txID <- view _thisTXID
  -- We hash with the transaction ID so that each new version reflects how
  -- it was created.  If the transaction is a known quantity, then this
  -- ensures that the version accurately reflects its effects and not those
  -- of some other, hidden, transaction.
  let newVer = mkVersionID (ver, txID)
  _escrowMap . at entID .= ((,newVer) . makeEscrowAbstract <$> gConcM)
  return y

-- | Registers a contract as a new escrow, returning its ID.  The argument
-- is a list of Haskell values marked as "bearers" of escrow-backed Fae
-- value; their backing escrows are transferred into the new escrow, so
-- that the bearer is no longer valuable in the contract that calls this
-- function.  The returned ID is completely determined by the contract in
-- which the escrow is created.
newEscrow :: 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType,
    MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m (EscrowID argType valType)
newEscrow eIDs f = liftTX $ Fae $ internalNewEscrow eIDs $ (getFae . f)

-- | Registers a contract publicly, with the same interface as 'newEscrow'.
-- Public contracts may only be called as transaction inputs with string
-- literal arguments, which are parsed into the actual 'argType' by the
-- provided 'Read' instance.
newContract ::
  (
    HasEscrowIDs argType, HasEscrowIDs valType, 
    Versionable argType, Versionable valType,
    Typeable argType, Typeable valType, 
    Read argType, NFData valType,
    MonadTX m
  ) =>
  [BearsValue] -> Contract argType valType -> m ()
newContract eIDs f = liftTX $ Fae $ do
  cAbs <- makeContractAbstract . makeConcrete <$> makeInternalT eIDs (getFae . f)
  tell [cAbs]

-- | Looks up a named signatory, maybe. 
lookupSigner :: (MonadTX m) => String -> m (Maybe PublicKey)
lookupSigner s = liftTX $ Fae $ view $ _txSigners . _getSigners . at s

-- | Looks up a named signatory, or throws if not found.
signer :: (MonadTX m) => String -> m PublicKey
signer s = fromMaybe (throw $ MissingSigner s) <$> lookupSigner s

-- | Returns the map of all signatories.
signers :: (MonadTX m) => m (Map String PublicKey)
signers = fmap getSigners $ liftTX $ Fae $ view _txSigners

-- | This function destroys a reward token, validating it in the process.
-- As the only interface to the `Reward` type, this /must/ be used by any
-- contract that intends to accept rewards as payment.
claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()


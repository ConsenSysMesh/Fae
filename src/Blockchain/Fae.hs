{-# LANGUAGE UndecidableInstances, Trustworthy #-}
{- |
Module: Blockchain.Fae
Description: A functional smart contract system
Copyright: (c) Ryan Reich, 2017-2018
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
    WithEscrows, EscrowID, BearsValue, RewardEscrowID, Reward,
    spend, release, useEscrow, newEscrow, 
    newContract, usingState, usingReader,
    lookupSigner, signer, signers, claimReward, bearer, 
    -- * Versioning
    -- | In order to ensure that transaction authors can rely on getting
    -- the escrow-backed values they expect, contract outputs are
    -- "versioned" and any change to any escrow ID alters the version.
    -- Contract literal arguments can refer to these values by version.
    Versioned(Versioned, getVersioned),
    -- * Opaque classes
    GetInputValues, HasEscrowIDs, Versionable, 
    -- * Re-exports
    Natural, Typeable, Exception, throw, evaluate, 
    Generic, Identity(..), Void
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Versions

import Common.Lens

import Control.Monad.Reader
import Control.Monad.State

import Control.Exception (Exception, throw, evaluate)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- * Types

-- | A contract transformer to apply effects to 'Fae'
type ContractM (t :: (* -> *) -> (* -> *)) argType valType =
  ContractT (t (Fae argType valType)) argType valType
-- | A transaction transformer like 'ContractM'
type TransactionM (t :: (* -> *) -> (* -> *)) a b = a -> t FaeTX b

-- | A simple utility for adding mutable state to a contract or
-- transaction, since the manual way of doing this is a little awkward.
usingState ::
  (Monad m) =>
  s ->
  (a -> StateT s m b) ->
  (a -> m b)
usingState s f = flip evalStateT s . f

-- | A simple utility for adding constant state to a contract or
-- transaction, since the manual way of doing this is a little awkward.
usingReader ::
  (Monad m) =>
  r ->
  (a -> ReaderT r m b) ->
  (a -> m b)
usingReader r f = flip runReaderT r . f


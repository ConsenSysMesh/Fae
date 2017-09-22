{- |
Module: Blockchain.Fae
Description: A functional smart contract system
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae is a functional smart contract system, a "functional alternative to Ethereum".  This module exposes the contract authoring API.

In Fae, a /contract/ is a function from "valued input" to "valued output".  The value is backed by /escrows/, which are contracts within contracts, and which are transferred along with the output they back when it is returned from the containing contract.  While escrows may be called by ID and input argument anywhere in contract code, a non-escrow contract (one that does not belong to any other contract) may only be called in the preamble to a /transaction/.  The reason for this will be explained later.

Contracts are modeled as /coroutines/, or functions that may be suspended and resumed.  Suspension, in the form of "releasing" a value, causes contract execution to end, returning the value, and causing the contract to be updated in storage to refer to the subsequent portion, which is resumed when the contract is next called.  Contracts are therefore entirely responsible for the nature of their persistent state, and may indeed be written in a monad with various state effects (reader, writer, and state being the obvious ones), or with none at all.

Contract storage is /lazy/, in that no entries are evaluated unless strictly required.  No activity within Fae strictly requires this evaluation, because ultimately, all contract executions are reflected in transaction results or new contracts, which are themselves stored lazily.  It is expected that parties interested in the contents of the storage will access it in a sandbox, only upon which will its entries finally be evaluated to the extent necessary.  Therefore, submitting new transactions to Fae is very cheap, as only a minimal amount of bookkeeping computation is done.

Unlike Ethereum, Fae has no computational fees ("gas").  The reason for this is that, because of the lazy design, one is never forced to execute untrusted code.  This is facilitated by the requirement that non-escrow contracts only be called in advance of transaction execution; in addition, the arguments to these contracts are limited to literals or outputs of trusted other contracts.  Therefore no untrusted code, either in the surrounding transaction or in the argument, intervenes in contract execution.

Fae is not a completely pure functional system: it has exceptions.  This is both because contract code, being Haskell, may throw exceptions for syntactic reasons, and because the system itself may raise exceptions.  Importantly, a transaction may not complete execution while any escrows remain open in it.  If an exception occurs anywhere within transaction execution, the transaction is voided and no updates to storage occur from it.  This facility is crucial for designing automatic trades within Fae, for which one should scrutinize "Blockchain.Fae.Contracts" as an example.
-}
module Blockchain.Fae
  (
    -- * Contract authors' API
    MonadContract(..), MonadTX(..), 
    release, spend, useEscrow, newEscrow, newContract, sender, bearer, 
    -- * Contract types
    Contract, ContractT, Transaction, Fae, FaeTX, BearsValue, HasEscrowIDs(..),
    -- * Identifier types
    PublicKey, ContractID, ShortContractID, EscrowID, AnEscrowID, 
    RewardEscrowID, anEscrowID, shorten, claimReward
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Transaction


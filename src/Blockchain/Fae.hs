module Blockchain.Fae
  (
    -- * Fae
    Fae, AnyFae, Contract, Transaction, Inputs, sender,
    -- * Contract
    ContractID, ShortContractID, spend, release, newContract,
    -- * Escrow
    EscrowID, AnEscrowID, HasEscrowIDs(..), newEscrow, useEscrow,
    -- * Rewards
    Reward, RewardEscrowID, claimReward
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads


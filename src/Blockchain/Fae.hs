module Blockchain.Fae
  (
    -- * Fae
    AnyFae, Fae, sender,
    -- * Contract
    ContractID, spend, outputContract, inputValue,
    -- * Escrow
    EscrowID, entryID, open, close, returnEscrow,
    -- * Rewards
    Reward, RewardEscrow, RewardEscrowID, claimReward
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Reward


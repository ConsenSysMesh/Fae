module Blockchain.Fae
  (
    -- * Fae
    AnyFae, Fae, sender,
    -- * Contract
    ContractID, spend, outputContract, inputValue,
    -- * Escrow
    EscrowID, open, close, returnEscrow,
    -- * Rewards
    Reward, RewardEscrow, RewardEscrowID, claimReward,
    -- * Overloaded functions
    FaeReturn, IsEscrow, Close
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Reward


module Blockchain.Fae
  (
    -- * Fae
    AnyFae, Fae, sender,
    -- * Contract
    ContractID, ShortContractID, spend, outputContract, inputValue,
    -- * Escrow
    Escrow, PrivateEscrow, EscrowID, AnyEscrowID(..), PrivateEscrowID,
    open, close, returnEscrow, transferEscrow,
    -- * Rewards
    Reward, RewardEscrow, RewardEscrowID, claimReward,
    -- * Overloaded functions
    FaeReturn, IsEscrow, HasEntryID
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Reward


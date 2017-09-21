module Blockchain.Fae
  (
    -- * Fae
    Fae, FaeTX, ContractT, Contract, Transaction, Inputs, sender,
    MonadContract(..), MonadTX(..),
    -- * Contract
    ContractID, ShortContractID,
    -- * Escrow
    WithEscrows, EscrowID, AnEscrowID, anEscrowID, HasEscrowIDs(..), 
    BearsValue, bearer,
    -- * Rewards
    Reward, RewardEscrowID, claimReward
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Transaction


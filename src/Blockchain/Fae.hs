module Blockchain.Fae
  (
    -- * Fae
    Fae, AnyFae, Contract, Contract', Transaction, Inputs, sender,
    MonadContracts(..), MonadFae(..), MonadFae'(..),
    -- * Contract
    ContractID, ShortContractID,
    -- * Escrow
    EscrowID, AnEscrowID, anEscrowID, HasEscrowIDs(..), 
    BearsValue, bearer,
    -- * Rewards
    Reward, RewardEscrowID, claimReward
  ) where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.Monads


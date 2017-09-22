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


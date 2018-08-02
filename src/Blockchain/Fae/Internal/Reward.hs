{- |
Module: Blockchain.Fae.Internal.Reward
Description: Fae rewards
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae does not have an associated currency, but it does provide a very
minimal valuable to certain transactions that can be exchanged for currency
or other derived values.
-}
module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.IDs

import Data.Serialize (Serialize)

import GHC.Generics

-- * Types

-- | The system-managed reward value.  Not constructible directly.
data RewardValue = Reward deriving (Generic)
-- | Private token controlling a 'RewardEscrowID'.
data RewardToken = Token deriving (Generic)
-- | The identifier for reward escrows
data Reward = RewardName deriving (Generic)
-- | The escrow ID of a reward token provided by the system.
type RewardEscrowID = EscrowID Reward

instance Serialize RewardValue
instance Serialize RewardToken
instance Serialize Reward

instance ContractName Reward where
  type ArgType Reward = RewardToken
  type ValType Reward = RewardValue
  theContract RewardName = rewardContract

-- | This is global to support the 'ContractName' instance
rewardContract :: Contract RewardToken RewardValue
rewardContract Token = spend Reward

-- | This function destroys a reward token, validating it in the process.
-- As the only interface to the `Reward` type, this /must/ be used by any
-- contract that intends to accept rewards as payment.
claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()


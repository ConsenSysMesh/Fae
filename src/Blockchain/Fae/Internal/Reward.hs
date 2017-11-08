{- |
Module: Blockchain.Fae.Internal.Reward
Description: Fae rewards
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae does not have an associated currency, but it does provide a very minimal valuable to certain transactions that can be exchanged for currency or other derived values.
-}
module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq

import GHC.Generics

-- * Types

-- | The system-managed reward value.  Not constructible directly.
data Reward = Reward deriving (Generic)
-- | Private token controlling a 'RewardEscrowID'.
data RewardToken = Token deriving (Generic)
-- | The escrow ID of a reward token provided by the system.
type RewardEscrowID = EscrowID RewardToken Reward 

-- | So that 'RewardEscrowID' is also 'NFData', which is necessary for
-- spending.
instance NFData RewardToken
-- | So that 'RewardEscrowID' is also 'NFData', which is necessary for
-- spending.
instance NFData Reward

-- | For the 'RewardEscrowID' instance
instance HasEscrowIDs RewardToken
-- | For the 'RewardEscrowID' instance
instance HasEscrowIDs Reward

-- * Functions

-- | How a reward is provided.  Users don't get this function, of course.
rewardEscrow :: Contract RewardToken Reward
rewardEscrow Token = spend Reward

-- | This function destroys a reward token, validating it in the process.
-- As the only interface to the `Reward` type, this /must/ be used by any
-- contract that intends to accept rewards as payment.
claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()


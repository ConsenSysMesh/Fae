module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq

import GHC.Generics

{- Types -}

-- | The system-managed reward value.  Not constructible directly.
data Reward = Reward deriving (Generic)
-- | Private token controlling a 'RewardEscrowID'.
data RewardToken = Token deriving (Generic)
-- | The escrow ID of a reward token provided by the system.
type RewardEscrowID = EscrowID RewardToken Reward 

instance NFData RewardToken
instance NFData Reward

instance HasEscrowIDs RewardToken
instance HasEscrowIDs Reward

{- Functions -}

rewardEscrow :: Contract RewardToken Reward
rewardEscrow Token = spend Reward

-- | This function destroys a reward token, validating it in the process.
-- As the only interface to the `Reward` type, this /must/ be used by any
-- contract that intends to accept rewards as payment.
claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()


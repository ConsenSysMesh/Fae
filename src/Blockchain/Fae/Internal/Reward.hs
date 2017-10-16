module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Storage

import Control.DeepSeq

import GHC.Generics

{- Types -}

data Reward = Reward deriving (Generic)
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

-- | This function destroys the reward token, validating it in the process.
claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()


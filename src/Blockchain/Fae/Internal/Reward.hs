module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Storage

{- Types -}

data Reward = Reward
data RewardToken = Token
type RewardEscrowID = EscrowID RewardToken Reward 

{- Functions -}

rewardEscrow :: (MonadTX m) => ContractT m RewardToken Reward
rewardEscrow Token = spend Reward

claimReward :: (MonadTX m) => RewardEscrowID -> m ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()



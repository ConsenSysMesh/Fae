module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Monads

import Data.Typeable

data Reward = Reward deriving (Typeable)
data Token = Token deriving (Typeable)

type RewardEscrow = Escrow Token Reward
type RewardEscrowID = EscrowID Token Reward

claimReward :: 
  RewardEscrowID -> Fae argType accumType valType -> Fae argType accumType valType
claimReward eID f = do
  Reward <- close eID Token
  f


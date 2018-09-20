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

import GHC.Generics

-- * Types

-- | The system-managed reward value.  Not constructible directly.
data RewardValue = Value deriving (Generic)
-- | Private token controlling a 'RewardEscrowID'.
data RewardToken = Token deriving (Generic)
-- | The identifier for reward escrows
data RewardName = RewardName deriving (Generic)
-- | An opaque type containing the escrow ID of a reward token provided by
-- the system.
newtype Reward = Reward (EscrowID RewardName) deriving (Generic)

-- | The argument and value types are both private types (not exported by
-- "Blockchain.Fae"), so that a reward contract cannot be called directly
-- by users; instead, they must use the interface, which consists only of
-- 'claimReward', which only destroys a reward and does not create them.
-- So the supply of 'Reward's is limited to those transactions that are
-- marked to receive them as special input values.
instance ContractName RewardName where
  type ArgType RewardName = RewardToken
  type ValType RewardName = RewardValue
  theContract RewardName = \Token -> spend Value

-- | This function destroys a reward token, validating it in the process.
-- As the only interface to the `Reward` type, this /must/ be used by any
-- contract that intends to accept rewards as payment.
claimReward :: (MonadTX m) => Reward -> m ()
claimReward (Reward eID) = do
  Value <- useEscrow [] eID Token
  return ()

-- | For the "Transaction" module, optionally fabricates a new reward token
-- and prepends it to the "input" results.  It goes without saying that
-- this should not be publicly visible.
withReward :: (MonadTX m) => Bool -> [ReturnValue] -> m [ReturnValue]
withReward isReward inputsL
  | isReward = do
      eID <- newEscrow RewardName
      return $ ReturnValue (Reward eID) : inputsL
  | otherwise = return inputsL


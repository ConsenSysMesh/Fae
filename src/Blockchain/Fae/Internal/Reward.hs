{- |
Module: Blockchain.Fae.Internal.Reward
Description: Fae rewards
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

Fae does not have an associated currency, but it does provide a very
minimal valuable to certain transactions that can be exchanged for currency
or other derived values.
-}
module Blockchain.Fae.Internal.Reward where

import Blockchain.Fae.Internal.IDs
import GHC.Generics

-- * Types

-- | The system-managed reward value.  Not constructible directly.
data Reward = Reward deriving (Generic)
-- | Private token controlling a 'RewardEscrowID'.
data RewardToken = Token deriving (Generic)
-- | The escrow ID of a reward token provided by the system.
type RewardEscrowID = EscrowID RewardToken Reward 

-- | For the 'RewardEscrowID' instance
instance HasEscrowIDs RewardToken
-- | For the 'RewardEscrowID' instance
instance HasEscrowIDs Reward


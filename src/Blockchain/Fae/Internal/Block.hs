{- |
Module: Blockchain.Fae.Internal.Block
Description: Block types and functions
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

In a blockchain, of course, transactions are distributed as blocks.  This has little to no effect on the semantics of individual contracts; the only relationship is the designation of reward transactions.  Otherwise, a block is just a sequence of transactions.
-}
module Blockchain.Fae.Internal.Block where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction
import Blockchain.Fae.Internal.TX

import Control.Monad.Trans

import Data.Sequence (Seq)

import GHC.Generics

-- * Types

-- | An ordering on a chunk of transactions, designating some to receive
-- rewards.
data Block =
  Block
  {
    rewardTransactions :: [TX],
    transactions :: [TX]
  }
  deriving (Generic)

-- * Instances

-- | Default instance, for 'Digestible'
instance Serialize Block
-- | Default instance, so that we can sign blocks
instance Digestible Block

-- * Functions

-- | Just maps 'interpretTX' over the list of transactions.  The reward
-- transactions come first.
runBlock :: (MonadMask m, MonadIO m) => Block -> FaeInterpretT m ()
runBlock Block{..} = do
  mapM_ (interpretTX True) rewardTransactions
  mapM_ (interpretTX False) transactions


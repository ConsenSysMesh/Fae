{- |
Module: FaeServer.History
Description: Stores and tracks successive Fae storage states
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The entry point to the Fae core library is just `interpretTX`, i.e. running
a single transaction with some preexisting storage state.  This state is
built up by @faeServer@'s event loop, but even that is not enough history,
because we need to be able to roll back to previous parent transactions
with the correct post-transaction state.  This module manages
a meta-history containing each such storage state for all transactions.
(Due to sharing of immutable values in Haskell, successive entries in this
history should only add an incremental amount of data, so the memory
requirements will not be quadratic as they may seem.)
-}

module FaeServer.History where

import Blockchain.Fae.FrontEnd

import Common.Lens ((&))

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import FaeServer.Concurrency
import FaeServer.Git

-- | Tracks all post-transaction states for the purpose of rolling back.
data TXHistory = 
  TXHistory
  {
    txStorageAndCounts :: Map TransactionID (Storage, Integer),
    bestTXID :: TransactionID,
    bestTXCount :: Integer
  }

-- | Monad for tracking history
type FaeInterpretWithHistoryT m = StateT TXHistory (FaeInterpretT m)

-- | Marshals the several parallel operations that need to occur when
-- a state rollback occurs: reset git /and/ fetch the old Fae storage.
recallHistory :: 
  (MonadIO m) => Maybe TransactionID -> FaeInterpretWithHistoryT m Integer
recallHistory parentM = do
  TXHistory{..} <- get
  let parent = fromMaybe bestTXID parentM
      err = error $ "No transaction in history with ID: " ++ show parent
  -- Weird construct forces this lookup before git runs
  (s, n) <- return $ Map.findWithDefault err parent txStorageAndCounts
  liftIO $ gitReset parent
  lift $ put s
  return n

-- | Not quite complementary to 'recallHistory', this merely updates the
-- 'TXHistory' but does not run git, which will operate differently
-- depending on transaction-specific parameters (e.g. @fake@).  It also
-- recomputes the "best", i.e. longest, chain of transactions, which is
-- used as the default parent for transactions that don't specify one.
--
-- The second, @Integer@ parameter gives the new transaction count; this is
-- necessary because there is a fencepost problem in how this is calculated
-- in the fast-forward from the transaction cache versus adding a new
-- transaction.
updateHistory :: 
  (Monad m) => Maybe TransactionID -> Integer -> FaeInterpretWithHistoryT m ()
updateHistory txIDM newCount = do
  TXHistory{..} <- get
  s <- lift get
  let txID = fromMaybe bestTXID txIDM
      txStorageAndCounts' = Map.insert txID (s, newCount) txStorageAndCounts
      (bestTXID', bestTXCount')
        | newCount > bestTXCount = (txID, newCount)
        | otherwise = (bestTXID, bestTXCount)
  put $ TXHistory txStorageAndCounts' bestTXID' bestTXCount'

-- | Updates with incremented count.
incrementHistory :: 
  (Monad m) => TransactionID -> Integer -> FaeInterpretWithHistoryT m ()
incrementHistory txID n = updateHistory (Just txID) (n + 1)

-- | Runs the stack of monads with an empty history.  The transaction count
-- starts at 1, not 0.
runFaeInterpretWithHistory :: 
  (MonadMask m, MonadIO m) => FaeInterpretWithHistoryT m () -> m ()
runFaeInterpretWithHistory = runFaeInterpret . flip evalStateT emptyTXHistory where
  emptyTXHistory = 
    TXHistory
    {
      txStorageAndCounts = 
        Map.singleton nullID (Storage Map.empty Map.empty, 1),
      bestTXID = nullID,
      bestTXCount = 1
    }


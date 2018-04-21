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

recallHistory :: 
  (MonadIO m) => Maybe TransactionID -> FaeInterpretWithHistoryT m Integer
recallHistory parentM = do
  TXHistory{..} <- get
  let parent = fromMaybe bestTXID parentM
  let err = error $ "No transaction in history with ID: " ++ show parent
  -- Weird construct forces this lookup before git runs
  (s, n) <- return $ Map.findWithDefault err parent txStorageAndCounts
  liftIO $ gitReset parent
  lift $ put s
  return n

updateHistory :: 
  (MonadIO m) => TransactionID -> Integer -> FaeInterpretWithHistoryT m ()
updateHistory txID txCount = do
  TXHistory{..} <- get
  s <- lift get
  let newCount = txCount + 1
  let txStorageAndCounts' = Map.insert txID (s, newCount) txStorageAndCounts
  let (bestTXID', bestTXCount')
        | txCount == bestTXCount = (txID, newCount)
        | otherwise = (bestTXID, bestTXCount)
  liftIO $ gitCommit txID
  put $ TXHistory txStorageAndCounts' bestTXID' bestTXCount'

runFaeInterpretWithHistory :: 
  (MonadMask m, MonadIO m) => FaeInterpretWithHistoryT m () -> m ()
runFaeInterpretWithHistory = runFaeInterpret . flip evalStateT emptyTXHistory 

  where
    emptyTXHistory = 
      TXHistory
      {
        txStorageAndCounts = Map.singleton nullID (Storage Map.empty, 1),
        bestTXID = nullID,
        bestTXCount = 1
      }


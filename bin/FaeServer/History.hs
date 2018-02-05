module FaeServer.History where

import Blockchain.Fae.FrontEnd

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

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
type FaeInterpretWithHistory = StateT TXHistory FaeInterpret

recallHistory :: Maybe TransactionID -> FaeInterpretWithHistory Integer
recallHistory parentM = do
  TXHistory{..} <- get
  let parent = fromMaybe bestTXID parentM
  let err = error $ "No transaction in history with ID: " ++ show parent
  -- Weird construct forces this lookup before git runs
  (s, n) <- return $ Map.findWithDefault err parent txStorageAndCounts
  liftIO $ gitReset parent
  liftFaeStorage $ put s
  return n

updateHistory :: TransactionID -> Integer -> FaeInterpretWithHistory ()
updateHistory txID txCount = do
  TXHistory{..} <- get
  s <- liftFaeStorage get
  let newCount = txCount + 1
  let txStorageAndCounts' = Map.insert txID (s, newCount) txStorageAndCounts
  let (bestTXID', bestTXCount')
        | txCount == bestTXCount = (txID, newCount)
        | otherwise = (bestTXID, bestTXCount)
  liftIO $ gitCommit txID
  put $ TXHistory txStorageAndCounts' bestTXID' bestTXCount'

liftFaeStorage :: FaeStorage a -> FaeInterpretWithHistory a
liftFaeStorage = lift . lift

runFaeInterpretWithHistory :: FaeInterpretWithHistory () -> IO ()
runFaeInterpretWithHistory = runFaeInterpret . flip evalStateT emptyTXHistory where
  emptyTXHistory = 
    TXHistory
    {
      txStorageAndCounts = Map.singleton nullID (Storage Map.empty, 0),
      bestTXID = nullID,
      bestTXCount = 0
    }




module Lottery 
  (
    Lottery(..), LotteryAction(..), LotteryResult(..), Nametags
  ) where

import Blockchain.Fae

import Control.Monad
import Control.Monad.Trans.State

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Nametag

data Lottery = 
  Lottery 
  {
    limit :: Natural
  }
  deriving (Generic)

data LotteryAction = Enter | Exit deriving (Generic, Read)

data LotteryResult =
  EnterResult 
  { 
    enterCount :: !Natural,
    message :: !String
  } |
  ExitResult 
  {
    lotteryLimit :: Natural,
    returned :: !Nametags
  } |
  WinResult
  {
    winnersCount :: Natural,
    returned :: !Nametags
  }
  deriving (Generic)

data LotteryState = 
  RunningState
  {
    nametags :: Entries,
    count :: Natural,
    winningCount :: Natural
  } |
  FinishedState
  {
    winners :: Entries,
    totalWinners :: Natural,
    nonWinners :: Entries
  }

type Entries = Map PublicKey (Seq Nametag)
type Nametags = Container (Seq Nametag)

startingState :: LotteryState
startingState = RunningState Map.empty 0 0

addEntry :: Nametag -> PublicKey -> Entries -> (Natural, Entries)
addEntry tag = Map.alterF $ finish . maybe (Seq.singleton tag) (tag Seq.<|)
  where finish set = (fromIntegral $ Seq.length set, Just set)

takeEntry :: PublicKey -> Entries -> (Maybe Nametags, Entries)
takeEntry = Map.alterF $ (,Nothing) . fmap Container

splitEntries :: Natural -> Entries -> (Entries, Entries)
splitEntries n = Map.partition ((== n) . fromIntegral . Seq.length)

instance ContractName Lottery where
  type ArgType Lottery = LotteryAction
  type ValType Lottery = LotteryResult
  theContract Lottery{..} = usingState startingState $ feedback $ \case
    Enter -> newEntry limit
    Exit -> getEntry limit

newEntry :: 
  Natural -> StateT LotteryState (Fae LotteryAction LotteryResult) LotteryResult
newEntry limit = do
  lotteryState <- get
  case lotteryState of
    FinishedState{} -> error "The lottery is finished; no new entries accepted"
    RunningState{..} -> do
      nametag <- material "nametag"
      message <- checkNametag nametag
      entrant <- signer "self"
      let (enterCount, nametags') = addEntry nametag entrant nametags
          count' = count + 1
          winningCount'
            | enterCount > winningCount = enterCount
            | otherwise = winningCount
      put $ 
        if count' == limit
        then let (winners, nonWinners) = splitEntries winningCount' nametags' in
             FinishedState{totalWinners = fromIntegral $ Map.size winners, ..}
        else RunningState
             {
               nametags = nametags', 
               count = count', 
               winningCount = winningCount'
             }
      return $! EnterResult{..}

getEntry :: 
  Natural -> StateT LotteryState (Fae LotteryAction LotteryResult) LotteryResult
getEntry lotteryLimit = do
  lotteryState <- get
  case lotteryState of
    RunningState{} -> error "The lottery is still running; no exits allowed"
    s@FinishedState{..} -> do
      entrant <- signer "self"
      let (wSetM, winners') = takeEntry entrant winners
          (nwSetM, nonWinners') = takeEntry entrant nonWinners
      case (wSetM, nwSetM) of
        (Nothing, Nothing) -> error "Not an entrant or already exited"
        (Just _, Just _) -> error "Internal error; entrant is in both maps"
        (Just returned, _) -> do
          put $ s{winners = winners'}
          return $! WinResult{winnersCount = totalWinners, ..}
        (_, Just returned) -> do
          put $ s{nonWinners = nonWinners'}
          return $! ExitResult{..}


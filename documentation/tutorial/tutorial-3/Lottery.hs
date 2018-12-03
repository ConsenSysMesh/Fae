module Lottery 
  (
    Lottery(..), LotteryAction(..), LotteryResult(..), Nametags
  ) where

import Blockchain.Fae

import Control.Monad
import Control.Monad.Trans.State

import Data.Maybe

import Nametag

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Lottery = 
  Lottery 
  {
    limit :: Natural,
    owner :: PublicKey
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

data LotteryError = NotAuthorized | Finished | NotFound | BothMaps

instance Show LotteryError where
  show NotAuthorized = "This action was not authorized by the lottery owner"
  show Finished = "The lottery is finished; no new entries accepted"
  show NotFound = "Not an entrant or already exited"
  show BothMaps = "Internal error; entrant is in both maps"

instance Exception LotteryError

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
    Enter -> newEntry limit owner
    Exit -> getEntry limit

newEntry :: 
  Natural -> PublicKey ->
  StateT LotteryState (Fae LotteryAction LotteryResult) LotteryResult
newEntry limit owner = do
  signedBy <- signer "owner"
  unless (signedBy == owner) $ throw NotAuthorized

  lotteryState <- get
  case lotteryState of
    FinishedState{} -> throw Finished 
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
  entrant <- signer "self"
  lotteryState <- get
  (result, newState) <- case lotteryState of
    RunningState{..} -> do
      let (returnedM, nametags') = takeEntry entrant nametags
          returned = fromMaybe (throw NotFound) returnedM
          result = ExitResult{..}
          newState = lotteryState{nametags = nametags'}
      return (result, newState)
    FinishedState{..} -> do
      let (wSetM, winners') = takeEntry entrant winners
          (nwSetM, nonWinners') = takeEntry entrant nonWinners
      case (wSetM, nwSetM) of
        (Nothing, Nothing) -> throw NotFound 
        (Just _, Just _) -> throw BothMaps 
        (Just returned, _) -> do
          let result = WinResult{winnersCount = totalWinners, ..}
              newState = lotteryState{winners = winners'}
          return (result, newState)
        (_, Just returned) -> do
          let result = ExitResult{..}
              newState = lotteryState{nonWinners = nonWinners'}
          return (result, newState)
  put newState
  return $! result


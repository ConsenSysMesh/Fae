module Auction where

import Blockchain.Fae
import Blockchain.Fae.Currency

import Control.Monad.State

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe

data AuctionAction = Bid | Collect deriving (Read)
data AuctionResult coin a = BidAccepted | Remit coin | Prize a deriving (Generic)

data AuctionError =
  NoBids | Can'tBid | Can'tCollect | MustBeat Natural | 
  UnauthorizedSeller PublicKey
  deriving (Show)

instance Exception AuctionError

data AuctionState coin = 
  BidState
  {
    bids :: Container (Map PublicKey coin),
    highBid :: Valuation coin,
    seller :: PublicKey,
    bidsLeft :: Natural
  } |
  CollectState (Container (Map PublicKey coin))
  deriving (Generic)

auction :: 
  (ContractVal a, Currency coin, MonadTX m) =>
  a -> Valuation coin -> Natural -> m ()
auction _ _ 0 = throw NoBids
auction x bid0 maxBids = do
  seller <- signer "self" 
  let state0 = BidState (Container Map.empty) bid0 seller maxBids
  newContract $ Auction state0 x
  
data Auction coin a = Auction (AuctionState coin) a deriving (Generic)

instance (ContractVal a, Currency coin) => ContractName (Auction coin a) where
  type ArgType (Auction coin a) = AuctionAction
  type ValType (Auction coin a) = AuctionResult coin a
  theContract (Auction state0 x) = usingState state0 $ feedback $ \act -> do
    aState <- get
    case (aState, act) of
      (CollectState{}, Bid) -> throw Can'tBid
      (BidState{}, Bid) -> bidStage x
      (_, Collect) -> collectStage

bidStage :: 
  (ContractVal a, Currency coin, MonadTX m) => 
  a -> StateT (AuctionState coin) m (AuctionResult coin a)
bidStage x = do
  -- Because of this, we have to assure that 'BidState' is the only
  -- possible constructor.
  BidState{..} <- get
  -- The seller has to sign on to a bid, lest someone pass a malicious
  -- "coin"
  claimedSeller <- signer "seller"
  unless (claimedSeller == seller) $ throw (UnauthorizedSeller claimedSeller)
  -- We look for an amount supplied by the bidder to /raise/ their previous
  -- bid (starting from 0), because we hold on to the bids until the
  -- auction is finished.
  raise <- material "raise"
  -- Look up the bidder's previous bid, if any, and add the new coin amount
  -- to get the new bid
  bidder <- signer "self"

  let bidsMap = getContainer bids
      oldBidM = Map.lookup bidder bidsMap
  newBidCoin <- maybe (return raise) (add raise) oldBidM
  newBid <- value newBidCoin
  -- Make sure they are actually raising
  unless (newBid > highBid) $ throw (MustBeat $ fromIntegral highBid)
  let bidsLeft' = bidsLeft - 1
  if (bidsLeft' > 0) 
  then do
    let bidsMap' = Map.insert bidder newBidCoin bidsMap
    put $ BidState (Container bidsMap') newBid seller bidsLeft'
    return BidAccepted
  else do
    put $ CollectState $ Container $
      -- The winning bid is earmarked for the seller
      Map.insert seller newBidCoin $
      -- The winning bidder doesn't get money back
      Map.delete bidder bidsMap
    return $ Prize x

collectStage ::
  (Currency coin, HasEscrowIDs a, MonadTX m) => 
  StateT (AuctionState coin) m (AuctionResult coin a)
collectStage = do
  sender <- signer "self"
  aState <- get
  Remit <$> case aState of
    BidState{bids = Container bidsMap,..} -> do
      let bid = Map.findWithDefault (throw Can'tCollect) sender bidsMap
      bidVal <- value bid
      when (bidVal == highBid) $ throw Can'tCollect
      let bidsMap' = Map.delete sender bidsMap
      put aState{bids = Container bidsMap'}
      return bid
    CollectState (Container bidsMap) -> do
      let bid = Map.findWithDefault (throw Can'tCollect) sender bidsMap
          bidsMap' = Map.delete sender bidsMap
      put $ CollectState $ Container bidsMap'
      return bid

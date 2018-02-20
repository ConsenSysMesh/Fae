module Auction where

import Blockchain.Fae
import Blockchain.Fae.Currency

import Control.Monad.State

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe

data AuctionError =
  NoBids | MustBid | Can'tBid | Can'tRemit | AlreadyGot | 
  MustBeat Natural | UnauthorizedSeller PublicKey
  deriving (Show)

instance Exception AuctionError

data AuctionState coin = 
  BidState
  {
    bids :: Map PublicKey coin,
    highBid :: Valuation coin,
    seller :: PublicKey,
    bidsLeft :: Natural
  } |
  RemitState (Map PublicKey (Maybe coin))

auction :: 
  (NFData a, Versionable a, HasEscrowIDs a, Currency coin, MonadTX m) =>
  a -> Valuation coin -> Natural -> m ()
auction _ _ 0 = throw NoBids
auction x bid0 maxBids = do
  seller <- signer "self" 
  let state0 = BidState Map.empty bid0 seller maxBids
  newContract [bearer x] $ flip evalStateT state0 . auctionC x

auctionC ::
  (Typeable a, HasEscrowIDs a, Currency coin) =>
  a -> 
  ContractM (StateT (AuctionState coin)) 
    (Maybe (Versioned coin))
    (Maybe (Either (Versioned coin) (Versioned a)))
auctionC x bidM = do
  bidStage bidM 
  -- The last bidder was the one with the highest (winning) bid, so they
  -- get the prize
  next <- release (Just $ Right $ Versioned x) 
  remitStage next
  
bidStage :: 
  (Typeable a, Currency coin, HasEscrowIDs a) => 
  Maybe (Versioned coin) ->
  StateT 
    (AuctionState coin) 
    (Fae (Maybe (Versioned coin)) (Maybe (Either (Versioned coin) (Versioned a))))
    ()
bidStage Nothing = throw MustBid
bidStage (Just (Versioned inc)) = do
  BidState{..} <- get
  -- The seller has to sign on to a bid, lest someone pass a malicious
  -- "coin"
  claimedSeller <- signer "seller"
  unless (claimedSeller == seller) $ throw (UnauthorizedSeller claimedSeller)

  -- Look up the bidder's previous bid, if any, and add the new coin amount
  -- to get the new bid
  bidder <- signer "self"
  let oldBidM = Map.lookup bidder bids
  newBidCoin <- maybe (return inc) (add inc) oldBidM
  newBid <- value newBidCoin
  -- Make sure they are actually raising
  unless (newBid > highBid) $ throw (MustBeat $ fromIntegral highBid)
  let bidsLeft' = bidsLeft - 1
  if (bidsLeft' > 0) 
  -- Loop
  then do
    let bids' = Map.insert bidder newBidCoin bids
    put $ BidState bids' newBid seller bidsLeft'
    next <- release Nothing 
    bidStage next
  -- Move on to the awards
  else put $ RemitState $ fmap Just $ 
    -- The winning bid is earmarked for the seller
    Map.insert seller newBidCoin $
    -- The winning bidder doesn't get money back
    Map.delete bidder bids

remitStage ::
  (Typeable a, Currency coin, HasEscrowIDs a) => 
  ContractM 
    (StateT (AuctionState coin)) 
    (Maybe (Versioned coin)) 
    (Maybe (Either (Versioned coin) (Versioned a)))
remitStage Nothing = do
  RemitState remits <- get
  sender <- signer "self"
  let 
    -- Look up the sender to see if they are owed some money
    bid =  
      fromMaybe (throw AlreadyGot) $
      fromMaybe (throw Can'tRemit) $
      Map.lookup sender remits
  -- Disable them from getting more money
  put $ RemitState $ Map.insert sender Nothing remits
  -- Remit
  next <- release (Just $ Left $ Versioned bid)
  -- Loop.  This contract never terminates, even after all the money is
  -- returned to the losers and paid to the seller.
  remitStage next
remitStage _ = throw Can'tBid

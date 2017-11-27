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
  (Versionable a, HasEscrowIDs a, Currency coin, MonadTX m) =>
  a -> Valuation coin -> Natural -> m ()
auction _ _ 0 = throw NoBids
auction x bid0 maxBids = do
  seller <- signer "self" 
  let state0 = BidState Map.empty bid0 seller maxBids
  newContract [bearer x] $ flip evalStateT state0 . auctionC seller x

auctionC ::
  (HasEscrowIDs a, Currency coin) =>
  PublicKey -> a -> 
  ContractM (StateT (AuctionState coin)) 
    (Maybe (Versioned coin))
    (Maybe (Either (Versioned coin) (Versioned a)))
auctionC seller x bidM = do
  bidStage seller bidM 
  next <- release (Just $ Right $ Versioned x) 
  remitStage next
  
bidStage :: 
  (Currency coin, HasEscrowIDs a) => 
  PublicKey ->
  Maybe (Versioned coin) ->
  StateT 
    (AuctionState coin) 
    (Fae (Maybe (Versioned coin)) (Maybe (Either (Versioned coin) (Versioned a))))
    ()
bidStage _ Nothing = throw MustBid
bidStage seller (Just (Versioned inc)) = do
  claimedSeller <- signer "seller"
  unless (claimedSeller == seller) $ throw (UnauthorizedSeller claimedSeller)

  BidState{..} <- get
  bidder <- signer "self"
  let oldBidM = Map.lookup bidder bids
  newBidCoin <- maybe (return inc) (add inc) oldBidM
  newBid <- value newBidCoin
  unless (newBid > highBid) $ throw (MustBeat $ fromIntegral highBid)
  let bidsLeft' = bidsLeft - 1
  if (bidsLeft' > 0) 
  then do
    let bids' = Map.insert bidder newBidCoin bids
    put $ BidState bids' newBid seller bidsLeft'
    next <- release Nothing 
    bidStage seller next
  else put $ RemitState $ fmap Just $ 
    Map.insert seller newBidCoin $
    Map.delete bidder bids

remitStage ::
  (Currency coin, HasEscrowIDs a) => 
  ContractM 
    (StateT (AuctionState coin)) 
    (Maybe (Versioned coin)) 
    (Maybe (Either (Versioned coin) (Versioned a)))
remitStage Nothing = do
  RemitState remits <- get
  sender <- signer "self"
  let 
    bid =  
      fromMaybe (throw AlreadyGot) $
      fromMaybe (throw Can'tRemit) $
      Map.lookup sender remits
  put $ RemitState $ Map.insert sender Nothing remits
  next <- release (Just $ Left $ Versioned bid)
  remitStage next
remitStage _ = throw Can'tBid

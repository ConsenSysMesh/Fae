{-# LANGUAGE Trustworthy #-}
{- |
Module: Blockchain.Fae.Contracts
Description: A contract library for Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides some contracts that would seem to be of very common
and general utility in a smart contract economy.
-}
module Blockchain.Fae.Contracts 
  (
    -- * Two-party swap
    -- $twopartyswap
    twoPartySwap,
    -- * Direct selling
    -- $vendor
    sell, redeem,
    -- * Possession
    -- $possession
    signOver, deposit
  )
  where

import Blockchain.Fae
import Blockchain.Fae.Currency

import Control.Monad

import Data.Maybe

-- $twopartyswap
-- The two-party swap is a fully manual exchange of values between two
-- parties.  Each one has the chance to verify the other's offering before
-- approving the swap; if either disagrees, both can get their offering
-- back; if both agree, each one gets the other's.

-- | The exceptions that can arise during a swap
data TwoPartyException = 
  NotAParty | MustVote | AlreadyVoted | CantVote | AlreadyGot
  deriving (Show)
                             
-- | Of course
instance Exception TwoPartyException

-- | This function accepts both offerings on creation; the intention is
-- that it is created in a single transaction signed jointly by both
-- parties, who withdraw their values from their respective "accounts".
--
-- The result of calling @'twoPartySwap' itemA itemB@ in a transaction
-- signed by two parties "partyA" and "partyB"  is that a contract is
-- created with signature
--
-- > Contract (Maybe Bool) (Maybe (Either (Versioned a) (Versioned b)))
--
-- that possesses both items.  This contract has two phases; in each one,
-- the party performing the action must sign the transaction as "self".
--
-- 1. Approval: each party may submit an argument of the form @Just True@
-- or @Just False@ to indicate if they approve or, respectively, disapprove
-- of the swap.  Only one vote is allowed per party, and a successful vote
-- returns 'Nothing'.
--
-- 2. Retrieval: This phase begins when either one party votes 'False' or
-- both parties vote 'True'.  Each party may call the contract once with
-- 'Nothing' to retrieve the item they are owed.  If there was a 'False'
-- vote, they get their own item back; if not, they get the other's item.
-- Either way, "partyA" gets the 'Left' value and "partyB" gets the
-- 'Right'.
--
-- Once both items are returned, the contract is deleted.
twoPartySwap ::
  (
    HasEscrowIDs a, HasEscrowIDs b, 
    Versionable a, Versionable b,
    Typeable a, Typeable b,
    MonadTX m
  ) =>
  a -> b -> m ()
twoPartySwap x y = do
  partyA <- signer "partyA"
  partyB <- signer "partyB"
  newContract [bearer x, bearer y] $ twoPartySwapC partyA partyB x y

-- | Semantic labels
data Stages = Stage1 | Stage2

-- | Both parts have two stages, so this contract has four iterations.
twoPartySwapC :: 
  (HasEscrowIDs a, HasEscrowIDs b, Versionable a, Versionable b) =>
  PublicKey -> PublicKey -> 
  a -> b ->
  Contract (Maybe Bool) (Maybe (Either (Versioned a) (Versioned b)))
twoPartySwapC partyA partyB x y choice1 = do
  values <- part1 Stage1 choice1
  noChoice <- release Nothing
  part2 Stage1 noChoice values
  
  where
    -- Every time this contract is called, we have to abort if the caller
    -- is not one of the two parties.
    getPartySigner = do
      who <- signer "self"
      unless (who == partyA || who == partyB) $ throw NotAParty 
      return who
    -- Convenient abbreviations
    xRet = Left $ Versioned x
    yRet = Right $ Versioned y
    -- Collect the votes and determine the payouts
    part1 _ Nothing = throw MustVote
    part1 _ (Just False) = return (xRet, yRet)
    part1 Stage1 (Just True) = do
      sender1 <- getPartySigner
      choice2 <- release Nothing
      sender2 <- getPartySigner
      when (sender1 == sender2) $ throw AlreadyVoted
      part1 Stage2 choice2
    part1 Stage2 (Just True) = return (yRet, xRet)
    -- Accept requests to pay out and deliver appropriately.
    part2 _ (Just _) _ = throw CantVote
    part2 Stage1 Nothing (forA, forB) = do
      receiver1 <- getPartySigner
      let 
        orderedValues
          | receiver1 == partyA = (forA, forB)
          | otherwise = (forB, forA)
      noChoice <- release $ Just $ fst orderedValues
      receiver2 <- getPartySigner
      when (receiver1 == receiver2) $ throw AlreadyGot 
      part2 Stage2 noChoice orderedValues
    part2 Stage2 Nothing (_, ret) = spend $ Just ret

-- $vendor
-- Direct sales are contracts in which the seller delivers the product
-- automatically, allowing Fae-powered stores.  This requires the seller as
-- well as the buyer to sign the transaction (like the two-party swap),
-- which can be automated by whatever front-end interface manages the sale.

-- | In a vendor contract, the payment could be bad in one form or another,
-- or the seller may not have authorized the sale.
data VendorError =
  UnauthorizedSeller PublicKey |
  NotEnough |
  BadToken

-- | -
instance Show VendorError where
  show (UnauthorizedSeller pubkey) =
    "Signer with public key " ++ show pubkey ++ " is not the seller."
  show NotEnough = "Insufficient payment"
  show BadToken = "Payment token is invalid"

-- | -
instance Exception VendorError

-- | The first argument is an escrow-backed value to sell; the second is
-- its price; the third is the seller's public key.
--
-- The sales contract has signature
--
-- @
--  Contract 
--  (Versioned coin)
--  (Either 
--    (Versioned coin) 
--    (Versioned a, Maybe (Versioned coin))
--  )
-- @
--
-- and possesses the item for sale.  It has two phases:
-- 
-- 1. Purchase: the prospective buyer offers payment in the form of the
-- contract argument.  This is accepted unless either the seller did not
-- sign the transaction as "seller" or the payment is insufficient.  If
-- accepted, the item is returned to the buyer along with any change as
-- a 'Right' value.
--
-- 2. Collection: the seller calls the contract (providing an 'undefined'
-- argument; beware, any money offered will be lost), and, if the "self"
-- public key matches the actual seller's, the payment is returned to them
-- as a 'Left' value.
sell :: 
  forall a coin m.
  (
    HasEscrowIDs a, Typeable a, Versionable a, 
    Currency coin, MonadTX m
  ) =>
  a -> Valuation coin -> PublicKey -> m ()
sell x price seller = newContract [bearer x] sellC where
  sellC :: 
    Contract 
      (Versioned coin)
      (Either 
        (Versioned coin) 
        (Versioned a, Maybe (Versioned coin))
      )
  sellC (Versioned payment) = do
    claimedSeller <- signer "seller"
    unless (claimedSeller == seller) $ 
      throw $ UnauthorizedSeller claimedSeller
    changeM <- change payment price
    let (cost, remitM) = fromMaybe (throw NotEnough) changeM
    _ <- release $ Right (Versioned x, Versioned <$> remitM)
    sender <- signer "self"
    unless (sender == seller) $
      throw $ NotOwner sender
    spend $ Left $ Versioned cost

-- | This is very similar to 'sell' except that instead of accepting
-- a currency and making change, it accepts an opaque token and
-- a validation function.
redeem ::
  forall a b m m'.
  (
    HasEscrowIDs a, HasEscrowIDs b, 
    Versionable a, Versionable b, 
    Typeable a, Typeable b, 
    Read b, MonadTX m
  ) =>
  a -> (b -> Fae b (Either b a) Bool) -> PublicKey -> m ()
redeem x valid seller = newContract [bearer x] redeemC where
  redeemC :: Contract b (Either b a)
  redeemC tok = do
    claimedSeller <- signer "seller"
    unless (claimedSeller == seller) $ 
      throw $ UnauthorizedSeller claimedSeller
    ok <- valid tok
    unless ok $ throw BadToken
    _ <- release $ Right x
    sender <- signer "self"
    unless (sender == seller) $
      throw $ UnauthorizedSeller sender
    spend $ Left tok

-- $possession
-- A possession contract is simply one that marks a value as being owned by
-- a particular cryptographically-identified entity.

-- | The only kind of error that can arise in a possession contract is that
-- the caller is not entitled to the value.
data PossessionError = NotOwner PublicKey

-- | -
instance Show PossessionError where
  show (NotOwner pubkey) = 
    "Signer with public key " ++ show pubkey ++ 
    " is not the owner of the requested value"

-- | -
instance Exception PossessionError

-- | The first argument is a value to assign possession; the second is the
-- public key of the recipient.  A new contract is created that takes no
-- arguments (that is, takes @()@) and checks that "self" is the owner, in
-- which case it returns the value.
signOver ::
  forall a m.
  (HasEscrowIDs a, Versionable a, Typeable a, MonadTX m) =>
  a -> PublicKey -> m ()
signOver x owner = newContract [bearer x] signOverC where
  signOverC :: Contract () a
  signOverC _ = do
    who <- signer "self"
    unless (owner == who) $ throw $ NotOwner who
    spend x

-- | Sign over a value to a named owner.
deposit ::
  (HasEscrowIDs a, Versionable a, Typeable a, MonadTX m) =>
  a -> String -> m ()
deposit x name = do
  owner <- signer name
  signOver x owner


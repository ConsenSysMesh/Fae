{-# LANGUAGE Trustworthy #-}
{- |
Module: Blockchain.Fae.Contracts
Description: A contract library for Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides some contracts that would seem to be of very common and general utility in a smart contract economy.
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

import Control.Exception
import Control.Monad
import Control.Monad.State

import Control.Monad.Reader.Class

import Data.Bool

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Typeable

import GHC.Generics

import Numeric.Natural

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

-- | This contract accepts both offerings on creation; the intention is
-- that it is created in a single transaction signed jointly by both
-- parties, who withdraw their values from their respective "accounts".
twoPartySwap ::
  (
    HasEscrowIDs a, HasEscrowIDs b, 
    Versionable a, Versionable b,
    Typeable a, Typeable b,
    NFData a, NFData b,
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
-- automatically, allowing Fae-powered stores.  This exposes the seller to
-- the risk of accepting a nonterminating computation embedded in the
-- payment, so they should have some way of limiting their customers to
-- a trusted subset.

-- | The first argument is an escrow-backed value to sell; the second is
-- its price; the third is the seller's public key.  The contract has two
-- stages: first, it makes change for the payment and returns it with the
-- product; this requires that the seller have signed the transaction.
-- Second, it waits for the seller to retrieve the payment.
sell :: 
  forall a m tok coin.
  (
    HasEscrowIDs a, Typeable a, Versionable a, NFData a,
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
    NFData a, NFData b,
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
    unless ok $ throw $ BadToken
    _ <- release $ Right x
    sender <- signer "self"
    unless (sender == seller) $
      throw $ UnauthorizedSeller sender
    spend $ Left tok

-- $possession
-- A possession contract is simply one that marks a value as being owned by
-- a particular cryptographically-identified entity.

-- | The first argument is a value to assign possession; the second is the
-- public key of the recipient.  A new contract is created that takes no
-- arguments (that is, takes @()@) and checks that the sender is the owner,
-- in which case it returns the value.
signOver ::
  forall a m.
  (HasEscrowIDs a, NFData a, Versionable a, Typeable a, MonadTX m) =>
  a -> PublicKey -> m ()
signOver x owner = newContract [bearer x] signOverC where
  signOverC :: Contract () a
  signOverC _ = do
    who <- signer "self"
    unless (owner == who) $ throw $ NotOwner who
    spend x

-- | Sign over a value to a named owner.
deposit ::
  (HasEscrowIDs a, NFData a, Versionable a, Typeable a, MonadTX m) =>
  a -> String -> m ()
deposit x name = do
  owner <- signer name
  signOver x owner

data ContractsError =
  NotEnough | NotOwner PublicKey | UnauthorizedSeller PublicKey | BadToken
  deriving (Show)

instance Exception ContractsError


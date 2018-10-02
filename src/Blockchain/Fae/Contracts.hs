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
    twoPartySwap, TwoPartySwap(..),
    -- * Direct selling
    -- $vendor
    sell, --redeem,
    Sell(..), --Redeem(..),
    -- * Possession
    -- $possession
    signOver, deposit,
    SignOver(..)
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

-- | The exceptions that can arise during a swap, constraining the possible
-- flow of the decision and its consequences.
data TwoPartyException = 
  NotAParty | MustVote | AlreadyVoted | CantVote | AlreadyGot
  deriving (Show)
                             
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
twoPartySwap :: (ContractVal a, ContractVal b, MonadTX m) => a -> b -> m ()
twoPartySwap x y = do
  partyA <- signer "partyA"
  partyB <- signer "partyB"
  newContract $ TwoPartySwap partyA partyB x y

-- | Semantic labels
data Stages = Stage1 | Stage2

-- | Swaps two values of any types.
data TwoPartySwap a b = TwoPartySwap PublicKey PublicKey a b deriving (Generic)

instance (ContractVal a, ContractVal b) => ContractName (TwoPartySwap a b) where
  type ArgType (TwoPartySwap a b) = Maybe Bool
  type ValType (TwoPartySwap a b) = Maybe (Either a b)

  -- | Both parts have two stages, so this contract has four iterations.
  theContract (TwoPartySwap partyA partyB x y) = \choice1 -> do
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
      xRet = Left x
      yRet = Right y
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
--
-- These "contracts" are all actually escrows deposited in the name of the
-- seller.  The prospective buyer writes a transaction that the seller must
-- also sign, allowing them to obtain the escrow and provide the desired
-- form of payment.

-- | In a vendor contract, the payment could be bad in one form or another.
data VendorError =
  NotEnough |
  BadToken

instance Show VendorError where
  show NotEnough = "Insufficient payment"
  show BadToken = "Payment token is invalid"

instance Exception VendorError

-- | The first argument is a valuable to sell; the second is its price; the
-- third is the seller's public key.
--
-- The sales escrow has signature
--
-- @ Contract coin (a, Maybe coin) @
--
-- and possesses the item for sale.  It accepts a payment, verifies that it
-- is sufficient for the desired price, and deposits the correct amount
-- into an account for the seller, while returning the item for sale to the
-- caller, along with any change.
sell :: 
  forall a coin m.
  (ContractVal a, Currency coin, MonadTX m) =>
  a -> Valuation coin -> PublicKey -> m ()
sell x price seller = keyTo (Sell x price) seller

-- | The first type is the valuable being sold, for a price in the
-- denomination of the second type.
data Sell a coin = Sell a (Valuation coin) PublicKey deriving (Generic)

instance (ContractVal a, Currency coin) => ContractName (Sell a coin) where
  type ArgType (Sell a coin) = coin
  type ValType (Sell a coin) = (a, Maybe coin)

  theContract (Sell x price seller) = \payment -> do
    changeM <- change payment price
    let (cost, remitM) = fromMaybe (throw NotEnough) changeM
    signOver cost seller
    spend (x, remitM)

-- -- | This is very similar to 'sell' except that instead of accepting
-- -- a currency and making change, it accepts an opaque token and
-- -- a validation function.
-- redeem ::
--   forall a b m m'.
--   (ContractVal a, ContractVal b, ContractArg b, MonadTX m) =>
--   a -> (b -> Fae b a Bool) -> PublicKey -> m ()
-- redeem x valid seller = keyTo (Redeem x valid) seller
-- 
-- -- | The first type is the valuable that is awarded for a valid token of
-- -- the second type.  Validity is determined by the provided boolean function.
-- data Redeem a b = Redeem a (b -> Fae b a Bool) PublicKey deriving (Generic)
-- 
-- instance 
--   (ContractVal a, ContractVal b, ContractArg b) => 
--   ContractName (Redeem a b) where
-- 
--   type ArgType (Redeem a b) = b
--   type ValType (Redeem a b) = a
-- 
--   theContract (Redeem x valid seller) = \tok -> do
--     ok <- valid tok
--     unless ok $ throw BadToken
--     signOver tok seller
--     spend x

-- $possession
-- A possession contract is simply one that marks a value as being owned by
-- a particular cryptographically-identified entity.

-- | The only kind of error that can arise in a possession contract is that
-- the caller is not entitled to the value.
data PossessionError = NotOwner PublicKey

instance Show PossessionError where
  show (NotOwner pubkey) = 
    "Signer with public key " ++ show pubkey ++ 
    " is not the owner of the requested value"

instance Exception PossessionError

-- | The first argument is a value to assign possession; the second is the
-- public key of the recipient.  A new contract is created that takes no
-- arguments (that is, takes @()@) and checks that "self" is the owner, in
-- which case it returns the value.
signOver :: (ContractVal a, MonadTX m) => a -> PublicKey -> m ()
signOver x owner = newContract $ SignOver owner x 

-- | Similar to 'signOver', but the value that is deposited takes the same
-- public key as the deposit.
keyTo :: 
  (ContractVal name, ContractName name, MonadTX m) => 
  (PublicKey -> name) -> PublicKey -> m () 
keyTo f owner = do
  eID <- newEscrow $ f owner
  signOver eID owner

-- | You sign over a specific value type to an owner identified by their
-- public key.
data SignOver a = SignOver PublicKey a deriving (Generic)

instance (ContractVal a) => ContractName (SignOver a) where
  type ArgType (SignOver a) = ()
  type ValType (SignOver a) = a

  theContract (SignOver owner x) = \_ -> do
    who <- signer "self"
    unless (owner == who) $ throw $ NotOwner who
    spend x

-- | Sign over a value to a named owner.
deposit :: (ContractVal a, MonadTX m) => a -> String -> m ()
deposit x name = do
  owner <- signer name
  signOver x owner

-- | Key a value to a named owner.
assign ::
  (ContractVal name, ContractName name, MonadTX m) => 
  (PublicKey -> name) -> String -> m () 
assign f name = do
  owner <- signer name
  keyTo f owner


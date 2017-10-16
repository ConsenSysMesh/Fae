{-|
Module: Blockchain.Fae.Currency
Description: A typeclass for currency escrows in Fae
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The primary motivating example of a smart contract is a currency such as Bitcoin.  This module provides a typeclass for such contracts as well as a sample currency.
-}
module Blockchain.Fae.Currency 
  (
    Currency(..),
    Coin, Token, reward
  )
where

import Blockchain.Fae

import Control.Applicative

import Control.Monad.Reader.Class

import Data.Function
import Data.Maybe
import Data.Ord
import Data.Typeable

import GHC.Generics

import Numeric.Natural

-- | Interface for a currency type.  The token type needs to be specified
-- because, possibly, not all "encryptions" support all the below
-- "homomorphic" operations.  Note that no actual encryption needs to be
-- involved.
class 
  (
    Typeable tok, Typeable coin,
    HasEscrowIDs tok, HasEscrowIDs coin,
    Integral (Valuation tok coin)
  ) => 
  Currency tok coin where

  data Valuation tok coin

  -- | Almost an @Ord@ instance; for comparing coin values.  
  balance :: (MonadTX m) => EscrowID tok coin -> EscrowID tok coin -> m Ordering
  -- | Create a new value.  It is important that this function be strict in
  -- its first argument, so that we know that the caller actually has
  -- a token and not just @undefined@.  This means that the correct way to
  -- handle bad permissions is to throw an exception.
  mint :: (MonadTX m) => tok -> Valuation tok coin -> m (EscrowID tok coin)
  -- | Peek at the value inside.  The ID remains valid.  Careful!  For
  -- semantic correctness, this function must also validate the escrow to
  -- prove that it actually has value.
  value :: (MonadTX m) => EscrowID tok coin -> m (Valuation tok coin)
  -- | Close the given accounts and make a new one with their sum.
  add :: (MonadTX m) => EscrowID tok coin -> EscrowID tok coin -> m (EscrowID tok coin)
  -- | Take off the given amount and return it and the change if both are
  -- nonnegative, otherwise @empty@.
  change :: 
    (Alternative f, MonadTX m) =>
    EscrowID tok coin -> Valuation tok coin -> 
    m (f (EscrowID tok coin, EscrowID tok coin))
  -- | Partition the value into the given proportions and the remainder.
  split :: 
    (Traversable t, MonadTX m) =>
    EscrowID tok coin -> t Natural -> 
    m (t (EscrowID tok coin), EscrowID tok coin)

  -- | Equality of values
  balanced :: (MonadTX m) => EscrowID tok coin -> EscrowID tok coin -> m Bool
  balanced eID1 eID2 = (== EQ) <$> balance eID1 eID2

  -- | First value is greater than second
  beats :: (MonadTX m) => EscrowID tok coin -> EscrowID tok coin -> m Bool
  beats eID1 eID2 = (== GT) <$> balance eID1 eID2

  -- | First value is less than second
  loses :: (MonadTX m) => EscrowID tok coin -> EscrowID tok coin -> m Bool
  loses eID1 eID2 = (== LT) <$> balance eID1 eID2

  -- | Rounds a coin down to the nearest multiple of some number, returning
  -- this rounded coin and the remainder.
  round :: (MonadTX m) => 
    EscrowID tok coin -> Natural -> 
    m (Maybe (EscrowID tok coin), EscrowID tok coin)
  round eID n = do
    (l, r) <- split eID [n]
    return (listToMaybe l, r)

{- The basic example -}

-- | This opaque type is the value of our sample currency.
newtype Coin = Coin Natural deriving (Generic)

instance HasEscrowIDs Coin

-- @Spend@ exists so that we can get close the escrow and get its
-- contents.  @UnsafePeek@ skips the closing part, and is therefore
-- economically dangerous, since it breaks conservation of value.  We must
-- use it carefully.
--
-- We need @UnsafePeek@ to be unsafe so that we can validate the coin: only
-- a geniune coin can have a Coin value, since we don't export the
-- constructor.  But we don't want to have to spend a coin to validate it.
--
-- In general, we use refutable, partial patterns when @close@ing an escrow
-- so that the pattern matching errors turn into exceptions in the
-- contract.
-- 
-- | The opaque token for using this currency.  Only the 'Currency'
-- functions are given access to its constructors, since otherwise,
-- a malicious user could create their own coins.
data Token = Spend | UnsafePeek deriving (Generic)

instance HasEscrowIDs Token

instance Currency Token Coin where
  newtype Valuation Token Coin = CoinValuation Natural
    deriving (Eq, Ord, Num, Real, Enum, Integral)

  balance eID1 eID2 = do
    Coin n1 <- useEscrow eID1 UnsafePeek
    Coin n2 <- useEscrow eID2 UnsafePeek
    return $ compare n1 n2

  mint !_ (CoinValuation n) = newEscrow [] f where
    f :: Contract Token Coin
    f UnsafePeek = release coin >>= f
    f Spend = spend coin
    coin = Coin n

  value eID = do
    Coin n <- useEscrow eID UnsafePeek
    return $ CoinValuation n

  add eID1 eID2 = do
    Coin n1 <- useEscrow eID1 Spend
    Coin n2 <- useEscrow eID2 Spend
    eID <- mint Spend $ CoinValuation $ n1 + n2
    return eID

  change eID n = do
    m <- value eID
    case m >= n of
      True -> do
        Coin m <- useEscrow eID Spend
        amtID <- mint Spend n
        remID <- mint Spend $ CoinValuation m - n
        return $ pure (amtID, remID)
      False -> return empty

  split eID weights = do
    Coin n <- useEscrow eID Spend
    let 
      s = sum weights
      (q, r) = n `quotRem` s
    partIDs <- traverse (mint Spend . CoinValuation . (q *)) weights
    remID <- mint Spend $ CoinValuation r
    return (partIDs, remID)

-- | A contract to claim system rewards in exchange for Coin values.  The
-- one-to-one exchange rate is of course an example and probably not
-- actually desirable.
--
-- Note that there is no Currency instance for rewards: thus, rewards are
-- a unary counting value and can only be collected individually; in
-- addition, 'claimReward' is the only means of spending them, and it
-- destroys the reward token.  So this function can be seen as reifying
-- rewards as a true currency, albeit one that cannot necessarily be used
-- to claim rewards from anywhere else.
reward :: (MonadTX m) => RewardEscrowID -> m (EscrowID Token Coin)
reward eID = do
  claimReward eID 
  mint Spend 1 


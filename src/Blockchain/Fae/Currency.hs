{-# LANGUAGE Trustworthy #-}
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
    -- * The currency typeclass
    Currency(..),
    -- * Basic numeric currency
    Coin, reward
  )
where

import Blockchain.Fae

import Control.Applicative

import Control.Monad.Reader.Class

import Data.Function
import Data.Maybe
import Data.Ord
import Data.Typeable

import Numeric.Natural

-- | Interface for a currency type.  The token type needs to be specified
-- because, possibly, not all "encryptions" support all the below
-- "homomorphic" operations.  Note that no actual encryption needs to be
-- involved.
class 
  (Versionable coin, NFData coin, HasEscrowIDs coin, Integral (Valuation coin)) => 
  Currency coin where

  data Valuation coin

  -- | A mixed 'Ord'-style comparison
  valCompare :: (MonadTX m) => coin -> Valuation coin -> m Ordering
  -- | Peek at the value inside.  The ID remains valid.  Careful!  For
  -- semantic correctness, this function must also validate the escrow to
  -- prove that it actually has value.
  value :: (MonadTX m) => coin -> m (Valuation coin)
  -- | Close the given accounts and make a new one with their sum.
  add :: (MonadTX m) => coin -> coin -> m coin
  -- | Take off the given amount and return it and the change if both are
  -- nonnegative, otherwise @empty@.
  change :: 
    (Alternative f, Alternative f', MonadTX m) =>
    coin -> Valuation coin -> m (f (coin, f' coin))
  -- | Partition the value into the given proportions and the remainder.
  split :: 
    (Traversable t, Alternative f, MonadTX m) =>
    coin -> t Natural -> m (t coin, f coin)

  -- | A mixed 'Eq'-style comparison
  atLeast :: (MonadTX m) => coin -> Valuation coin -> m Bool
  atLeast c n = not <$> lessThan c n

  -- | Strict mixed 'Eq'-style comparison
  moreThan :: (MonadTX m) => coin -> Valuation coin -> m Bool
  moreThan c n = (== GT) <$> valCompare c n

  -- | Reversed strict mixed 'Eq'-style comparison
  lessThan :: (MonadTX m) => coin -> Valuation coin -> m Bool
  lessThan c n = (== LT) <$> valCompare c n

  -- | A pure 'Ord'-style comparison
  coinCompare :: (MonadTX m) => coin -> coin -> m Ordering
  coinCompare c1 c2 = do
    n <- value c2
    valCompare c1 n

  -- | A pure 'Eq'-style comparison
  matches :: (MonadTX m) => coin -> coin -> m Bool
  matches c1 c2 = not <$> losesTo c1 c2

  -- | A strict pure 'Eq'-style comparison
  beats :: (MonadTX m) => coin -> coin -> m Bool
  beats c1 c2 = (== GT) <$> coinCompare c1 c2

  -- | A reversed strict pure 'Eq'-style comparison
  losesTo :: (MonadTX m) => coin -> coin -> m Bool
  losesTo c1 c2 = (== LT) <$> coinCompare c1 c2

  -- | Rounds a coin down to the nearest multiple of some number, returning
  -- this rounded coin and the remainder.
  round :: (MonadTX m) => 
    coin -> Natural -> m (Maybe coin, coin)
  round c n = do
    (l, rM) <- split c [n]
    return (listToMaybe l, fromMaybe c rM)

{- The basic example -}

-- | This opaque type is the value of our sample currency.
newtype CoinVal = CoinVal Natural deriving (Generic)

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

-- | This is the actual currency; no user ever looks inside directly.
type Coin = EscrowID Token CoinVal

-- | This internal function is obviously not to be called by users.
mint :: (MonadTX m) => Natural -> m Coin
mint n = newEscrow [] f where
  f :: Contract Token CoinVal
  f UnsafePeek = release coin >>= f
  f Spend = spend coin
  coin = CoinVal n

instance Currency Coin where
  newtype Valuation Coin = CoinValuation Natural
    deriving (Eq, Ord, Num, Real, Enum, Integral)

  valCompare eID (CoinValuation n) = do
    CoinValuation m <- value eID
    return $ compare m n

  value eID = do
    CoinVal n <- useEscrow eID UnsafePeek
    return $ CoinValuation n

  add eID1 eID2 = do
    CoinVal n1 <- useEscrow eID1 Spend
    CoinVal n2 <- useEscrow eID2 Spend
    eID <- mint $ n1 + n2
    return eID

  change eID nV@(CoinValuation n) = do
    ord <- valCompare eID nV
    case ord of
      EQ -> return $ pure (eID, empty)
      GT -> do
        CoinVal m <- useEscrow eID Spend
        amtID <- mint n
        remID <- mint $ m - n
        return $ pure (amtID, pure remID)
      LT -> return empty

  split eID weights = do
    CoinVal n <- useEscrow eID Spend
    let 
      s = sum weights
      (q, r) = n `quotRem` s
    partIDs <- traverse (mint . (q *)) weights
    if r == 0
    then return (partIDs, empty)
    else do
      remID <- mint r
      return (partIDs, pure remID)

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
reward :: (MonadTX m) => RewardEscrowID -> m Coin
reward eID = do
  claimReward eID 
  mint 1 


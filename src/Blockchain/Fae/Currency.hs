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

import Numeric.Natural

-- | Interface for a currency type.  The token type needs to be specified
-- because, possibly, not all "encryptions" support all the below
-- "homomorphic" operations.  Note that no actual encryption needs to be
-- involved.
class Currency tok coin where
  -- | Almost an @Ord@ instance; for comparing coin values.  
  balance :: EscrowID tok coin -> EscrowID tok coin -> AnyFae Ordering
  -- | Create a new value.  It is important that this function be strict in
  -- its first argument, so that we know that the caller actually has
  -- a token and not just @undefined@.  This means that the correct way to
  -- handle bad permissions is to throw an exception.
  mint :: tok -> Natural -> AnyFae (EscrowID tok coin)
  -- | Peek at the value inside.  The ID remains valid.  Careful!  For
  -- semantic correctness, this function must also validate the escrow to
  -- prove that it actually has value.
  value :: EscrowID tok coin -> AnyFae Natural
  -- | Close the given accounts and make a new one with their sum.
  add :: EscrowID tok coin -> EscrowID tok coin -> AnyFae (EscrowID tok coin)
  -- | Take off the given amount and return it and the change if both are
  -- nonnegative, otherwise @empty@.
  change :: 
    (Alternative f) =>
    EscrowID tok coin -> Natural -> 
    AnyFae (f (EscrowID tok coin, EscrowID tok coin))
  -- | Partition the value into the given proportions and the remainder.
  split :: 
    (Traversable t) =>
    EscrowID tok coin -> t Natural -> 
    AnyFae (t (EscrowID tok coin), EscrowID tok coin)

  -- | Equality of values
  balanced :: EscrowID tok coin -> EscrowID tok coin -> AnyFae Bool
  balanced eID1 eID2 = (== EQ) <$> balance eID1 eID2

  -- | First value is greater than second
  beats :: EscrowID tok coin -> EscrowID tok coin -> AnyFae Bool
  beats eID1 eID2 = (== GT) <$> balance eID1 eID2

  -- | First value is less than second
  loses :: EscrowID tok coin -> EscrowID tok coin -> AnyFae Bool
  loses eID1 eID2 = (== LT) <$> balance eID1 eID2

  -- | Rounds a coin down to the nearest multiple of some number, returning
  -- this rounded coin and the remainder.
  round :: 
    EscrowID tok coin -> Natural -> 
    AnyFae (Maybe (EscrowID tok coin), EscrowID tok coin)
  round eID n = do
    (l, r) <- split eID [n]
    return (listToMaybe l, r)

{- The basic example -}

newtype Coin = Coin Natural deriving (Typeable)

-- | @Spend@ exists so that we can get close the escrow and get its
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
data Token = Spend | UnsafePeek deriving (Typeable)

instance Currency Token Coin where
  balance eID1 eID2 = do
    Coin n1 <- close eID1 UnsafePeek
    Coin n2 <- close eID2 UnsafePeek
    return $ compare n1 n2

  mint !_ !n = open [] $ do
    let coin = Coin n
    arg <- ask
    case arg of
      UnsafePeek -> return coin
      Spend -> do
        spend
        return coin

  value eID = do
    Coin n <- close eID UnsafePeek
    return n

  add eID1 eID2 = do
    Coin n1 <- close eID1 Spend
    Coin n2 <- close eID2 Spend
    eID <- mint Spend $ n1 + n2
    return eID

  change eID n = do
    m <- value eID
    case m >= n of
      True -> do
        Coin m <- close eID Spend
        amtID <- mint Spend n
        remID <- mint Spend $ m - n
        return $ pure (amtID, remID)
      False -> return empty

  split eID weights = do
    Coin n <- close eID Spend
    let 
      s = sum weights
      (q, r) = n `quotRem` s
    partIDs <- traverse (mint Spend . (q *)) weights
    remID <- mint Spend r
    return (partIDs, remID)

-- This value is of course just an example.  It's not much of an incentive.
reward :: RewardEscrowID -> AnyFae (EscrowID Token Coin)
reward eID = claimReward eID $ mint Spend 1 


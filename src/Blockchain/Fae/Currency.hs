{-# LANGUAGE Trustworthy #-}
{-|
Module: Blockchain.Fae.Currency
Description: A typeclass for currency escrows in Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The primary motivating example of a smart contract is a currency such as
Bitcoin.  This module provides a typeclass for currency values that can be safely exchanged through the Fae smart contract system.
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

import Control.Monad.State

import Data.Maybe

-- | Interface for a currency type.
class 
  (
    Versionable coin, ContractVal coin, 
    Integral (Valuation coin), 
    -- This one is because it should be reasonably common to have
    -- valuations inside of valuables, particularly 'ContractName's.
    HasEscrowIDs (Valuation coin)
  ) => 
  Currency coin where

  data Valuation coin

  -- | Like the name says.  Sometimes useful; should satisfy
  -- 
  -- prop> zero >>= value = return 0
  zero :: (MonadTX m) => m coin
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
  --
  -- Note that @split [a0 * b, a1 * b, ..]@ is not equivalent to @split
  -- [a0, a1, ..]@ despite the proportions being the same: each portion is
  -- given the designated number of integral "parts", so the former
  -- produces a list of multiples of @b@ and the latter need not.
  --
  -- In any case, the value being split is first rounded down to the
  -- nearest multiple of the sum of the portions, the difference being
  -- returned as the remainder.  If this rounding is all that is desired,
  -- use 'round' instead.
  split :: 
    (Traversable t, Alternative f, MonadTX m) =>
    coin -> t Natural -> m (t coin, f coin)
  split c ws 
    | s == 0 = do
        xs <- mapM (const zero) ws
        return (xs, pure c)
    | otherwise = do
        v <- value c
        let 
          q = v `quot` fromIntegral s
          vs = fmap (\w -> fromIntegral w * q) ws
          f Nothing _ = (, Nothing) <$> zero
          f (Just c) v = do
            qrM <- change c v
            maybe ((, Nothing) <$> zero) return qrM
        (xs, rM) <- mapMAccumL f (return $ Just c) vs
        return (xs, maybe empty pure rM)
    where 
      s = sum ws
      mapMAccumL ::
        (Monad m, Traversable t) =>
        (a -> b -> m (c, a)) -> m a -> t b -> m (t c, a)
      mapMAccumL g y0 xs = runStateT (mapM (StateT . flip g) xs) =<< y0

  -- | A mixed 'Ord'-style comparison
  valCompare :: (MonadTX m) => coin -> Valuation coin -> m Ordering
  valCompare c v = do
    cv <- value c
    return $ compare cv v

  -- | A mixed 'Eq'-style comparison
  atLeast :: (MonadTX m) => coin -> Valuation coin -> m Bool
  atLeast c n = not <$> lessThan c n

  -- | Strict mixed 'Eq'-style comparison
  moreThan :: (MonadTX m) => coin -> Valuation coin -> m Bool
  moreThan c n = (== GT) <$> valCompare c n

  -- | A mixed 'Eq'-style comparison
  atMost :: (MonadTX m) => coin -> Valuation coin -> m Bool
  atMost c n = not <$> moreThan c n

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

-- | Convenience type so that type signatures can say they accept the
-- semantically-meaningful 'Coin' rather than some escrow ID.
newtype Coin = Coin (EscrowID CoinName) deriving (Generic)
-- | Only for this module; it's not exported so it doesn't matter in
-- regular usage.
data CoinName = MintCoin Natural deriving (Generic)

-- | DRY shortcut, purely internal
mint :: (MonadTX m) => Natural -> m Coin
mint = fmap Coin . newEscrow . MintCoin

instance ContractName CoinName where
  type ArgType CoinName = Bool
  type ValType CoinName = Natural

  -- | The coin has two modes: report ('False') and withdraw ('True').
  -- Obviously this entire function is unsafe for users of 'Coin', because it
  -- allows them to create a new coin of any value.  It is, however, okay for
  -- them to have the type signature of the escrow function itself, because
  -- simply having an escrow of that signature is not enough to have
  -- a 'Coin'; the 'CoinName' is also required, and that is hidden.
  theContract name@(MintCoin n) False = release n >>= theContract name
  theContract (MintCoin n) True = spend n

instance Currency Coin where
  -- | The constructor isn't exported because using it is equivalent to
  -- using 'toInteger' and 'fromInteger', methods of 'Integral' and 'Num'
  -- respectively.  These operations are useful to construct a valuation of
  -- a particular "denonination", or extract the raw numerical value.
  newtype Valuation Coin = CoinValuation Natural
    deriving (Eq, Ord, Num, Real, Enum, Integral, Generic)

  zero = mint 0

  value (Coin eID) = CoinValuation <$> useEscrow eID False

  add (Coin eID1) (Coin eID2) = do
    n1 <- useEscrow eID1 True
    n2 <- useEscrow eID2 True
    mint $ n1 + n2

  change c@(Coin eID) nV@(CoinValuation n) = do
    ord <- valCompare c nV
    case ord of
      EQ -> return $ pure (c, empty)
      GT -> do
        m <- useEscrow eID True
        amt <- mint n
        rem <- mint $ m - n
        return $ pure (amt, pure rem)
      LT -> return empty

instance Show (Valuation Coin) where
  show (CoinValuation n) = show n

-- | A contract to redeem system rewards for 'Coin' values.  The one-to-one
-- exchange rate is of course an example and probably not actually
-- desirable.
--
-- Note that there is no 'Currency' instance for rewards: thus, rewards are
-- a unary counting value and can only be collected individually; in
-- addition, 'claimReward' is the only means of spending them, and it
-- destroys the reward token.  So this function can be seen as reifying
-- rewards as a true currency, albeit one that cannot necessarily be used
-- to claim rewards from anywhere else.
reward :: (MonadTX m) => RewardEscrowID -> m Coin
reward eID = do
  claimReward eID 
  mint 1


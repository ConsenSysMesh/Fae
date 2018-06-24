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
    -- | This is a nearly featureless currency that most likely suffers
    -- from the effects of inflation, since its reward function creates one
    CoinName(..), Coin, reward
  )
where

import Blockchain.Fae

import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.State

import Data.Function
import Data.Maybe
import Data.Ord
import Data.Serialize
import Data.Typeable

import Numeric.Natural

-- | Interface for a currency type.
class 
  (Versionable coin, ContractVal coin, Integral (Valuation coin)) => 
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

  -- | A pure 'Eq'-style comparison
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
type Coin = EscrowID CoinName
-- | Only for this module; it's not exported so it doesn't matter in
-- regular usage.
data CoinName = MintCoin Natural deriving (Generic)

-- | The coin has two modes: report ('False') and withdraw ('True').
-- Obviously this entire function is unsafe for users of 'Coin', because it
-- allows them to create a new coin of any value.  It is, however, okay for
-- them to have the type signature of the escrow function itself, because
-- simply having an escrow of that signature is not enough to have
-- a 'Coin'; the 'CoinName' is also required, and that is hidden.
coinContract :: CoinName -> Contract Bool Natural
coinContract name@(MintCoin n) False = release n >>= coinContract name
coinContract (MintCoin n) True = spend n

-- | Helpful shortcut
mint :: (MonadTX m) => Natural -> m Coin
mint = newEscrow [] . MintCoin

instance Serialize CoinName

instance ContractName CoinName where
  type ArgType CoinName = Bool
  type ValType CoinName = Natural
  theContract = coinContract

instance Currency Coin where
  newtype Valuation Coin = CoinValuation Natural
    deriving (Eq, Ord, Num, Real, Enum, Integral)

  zero = mint 0

  value eID = CoinValuation <$> useEscrow eID False

  add eID1 eID2 = do
    n1 <- useEscrow eID1 True
    n2 <- useEscrow eID2 True
    mint $ n1 + n2

  change eID nV@(CoinValuation n) = do
    ord <- valCompare eID nV
    case ord of
      EQ -> return $ pure (eID, empty)
      GT -> do
        m <- useEscrow eID True
        amtID <- mint n
        remID <- mint $ m - n
        return $ pure (amtID, pure remID)
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


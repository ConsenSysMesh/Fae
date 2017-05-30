{-# LANGUAGE RecursiveDo #-}
module TX (tx, BCoin) where

import Blockchain.Fae
import Blockchain.Fae.Contracts (verifySig)

-- 'Fae' is a 'MonadFix'.
import Control.Monad.Fix

import Data.Bifunctor

import Numeric.Natural

tx :: Fae [EntryID]
tx = mdo
  let idList = [spendID, addID, splitID, rewardID]
  spendID <- create All (spendBCoin idList) id
  addID <- create (Only idList) addBCoins value
  debitID <- create (Only idList) debitBCoin (bimap value)
  rewardID <- create All (rewardBCoin idList) id
  return idList

newtype BCoin = BCoin { value :: Natural }

spendBCoin :: [EntryID] -> (EscrowID BCoin, PublicKey) -> Fae EntryID
spendBCoin idList (coinID, key) = mdo
  coin <- close coinID
  newID <- create (Only idList) (verifySig coin key newID) value
  return newID

addBCoins :: (EscrowID BCoin, EscrowID BCoin) -> Fae BCoin
addBCoins (coin1ID, coin2ID) = do
  coin1 <- close coin1ID
  coin2 <- close coin2ID
  return $ BCoin $ value coin1 + value coin2

debitBCoin :: (EscrowID BCoin, Natural) -> Fae (BCoin, BCoin)
debitBCoin (coinID, debit) = do
  coin <- close coinID
  if debit <= value coin
  then return (BCoin $ value coin - debit, BCoin debit)
  else throw "Bad debit"

rewardBCoin :: [EntryID] -> EscrowID Reward -> Fae EntryID
rewardBCoin idList rewardID = mdo
  _ <- close rewardID
  let coin = BCoin $ 50 * 10^8 -- As in Bitcoin, initially
  key <- signer
  newID <- create (Only idList) (verifySig coin key newID) value
  return newID


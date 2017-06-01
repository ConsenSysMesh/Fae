{-# LANGUAGE RecursiveDo #-}
module TX (tx, BCoin) where

import Blockchain.Fae
import Blockchain.Fae.Crypto (verifySig)

import Numeric.Natural

-- | This transaction creates the API contracts for BCoin.  Their actual
-- entry IDs can be obtained by calling `follow` on the transaction ID once
-- it is executed.
tx :: Fae ()
tx = mdo
  let idList = [spendID, addID, splitID, rewardID]
  spendID <- label "spendBCoin" $ create (spendBCoin idList)
  addID <- label "addBCoins" $ create (addBCoins idList)
  debitID <- label "debitBCoin" $ create (debitBCoin idList)
  creator <- signer
  rewardID <- label "rewardBCoin" $ create (rewardBCoin idList creator) 
  return ()

-- | An opaque wrapper for a positive integral value.  Users will not be
-- able to construct this, preserving scarcity.
newtype BCoin = BCoin { value :: Natural }

-- | The first argument is evaluated during creation.
--   The second argument is a coin to spend and an identity to give it to.
spendBCoin :: 
  [EntryID] -> 
  (EscrowID BCoin, PublicKey) -> Fae EntryID
spendBCoin idList (coinID, key) = mdo
  coin <- close coinID
  newID <- label "BCoin" $ create (sigContract idList coin key newID)
  return newID

-- | The first argument is evaluated during creation.
--   The second argument is a pair of coins to add.
addBCoins :: 
  [EntryID] -> 
  (EscrowID BCoin, EscrowID BCoin) -> Fae (EscrowID BCoin)
addBCoins idList (coin1ID, coin2ID) = do
  coin1 <- close coin1ID
  coin2 <- close coin2ID
  let coin = BCoin $ value coin1 + value coin2
  escrow (Only idList) coin value 

-- | The first argument is evaluated during creation.
--   The second argument is a coin and a desired debit.
--   If the coin's balance is large enough, the debit occurs.
debitBCoin :: 
  [EntryID] -> 
  (EscrowID BCoin, Natural) -> Fae (Maybe (EscrowID BCoin, EscrowID BCoin))
debitBCoin idList (coinID, debit) = do
  v <- peek coinID
  if debit <= v
  then do
    _ <- close coinID
    eID1 <- escrow (Only idList) $ BCoin $ v - debit
    eID2 <- escrow (Only idList) $ BCoin debit
    return $ Just (eID1, eID2)
  else return Nothing

-- | The first two arguments are evaluated during creation.
--   The third argument, when a reward, creates an award coin.
--   When a signature, checks the signer against the contract creator and
--   deletes the contract.
rewardBCoin :: 
  [EntryID] -> PublicKey -> 
  Either Signature (EscrowID Reward) -> Fae (Maybe EntryID)
rewardBCoin idList _ (Right rewardID) = mdo
  _ <- close rewardID
  let coin = BCoin $ 50 * 10^8 -- As in Bitcoin, initially
  key <- signer
  newID <- label "BCoin" $ create (sigContract idList coin key newID)
  return $ Just newID

-- When the right signature is passed, terminate rewards.
rewardBCoin [_,_,_,self] creator (Left sig)
  | verifySig creator self sig = do
      spend
      return Nothing
  | otherwise = return Nothing

-- | Utility for creating a contract that authenticates a signature.
sigContract :: 
  [EntryID] -> BCoin -> PublicKey -> EntryID -> 
  Signature -> Fae (Maybe (EscrowID BCoin))
sigContract idList coin key entryID sig 
  | verifySig key entryID sig = do
      eID <- escrow (Only idList) coin
      return $ Just eID
  | otherwise = return Nothing


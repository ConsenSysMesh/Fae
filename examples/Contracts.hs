module Blockchain.Fae.Contracts
  (
    sigContract
  ) where

-- | Utility for creating a contract that authenticates a signature.
sigContract :: 
  [EntryID] -> a -> PublicKey -> EntryID -> 
  Signature -> Fae (Maybe (EscrowID a))
sigContract idList x key entryID sig 
  | verifySig key entryID sig = do
      eID <- escrow (Only idList) x
      return $ Just eID
  | otherwise = return Nothing


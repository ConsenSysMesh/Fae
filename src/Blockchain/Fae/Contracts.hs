module Blockchain.Fae.Contracts
  (
    sigContract
  ) where

import Blockchain.Fae
import Blockchain.Fae.Crypto

-- | Utility for creating a contract that authenticates a signature.
sigContract :: 
  a -> (a -> Fae EscrowID) -> PublicKey -> EntryID -> 
  Signature -> Fae (Maybe EscrowID)
sigContract x makeEscrow key entryID sig 
  | verifySig key entryID sig = do
      eID <- makeEscrow x
      return $ Just eID
  | otherwise = return Nothing


module Blockchain.Fae.Contracts
  (
    sigContract
  ) where

import Blockchain.Fae

-- | Utility for creating a contract that authenticates a signature.
sigContract :: 
  a -> (a -> EscrowID tok a b) -> PublicKey -> EntryID -> 
  Signature -> Fae (Maybe (EscrowID tok a b))
sigContract x makeEscrow key entryID sig 
  | verifySig key entryID sig = do
      eID <- makeEscrow x
      return $ Just eID
  | otherwise = return Nothing


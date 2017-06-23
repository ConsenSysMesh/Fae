module Blockchain.Fae.Contracts
  (
    sigContract
  ) where

import Blockchain.Fae
import Blockchain.Fae.Crypto

import Data.Typeable

-- | Utility for creating a contract that authenticates a signature.
sigContract :: 
  (Typeable tokT, Typeable pubT) =>
  a -> (a -> Fae (EscrowID tokT pubT a)) -> PublicKey -> EntryID -> 
  Signature -> Fae (Maybe (EscrowID tokT pubT a))
sigContract x makeEscrow key entryID sig 
  | verifySig key entryID sig = do
      eID <- makeEscrow x
      return $ Just eID
  | otherwise = return Nothing


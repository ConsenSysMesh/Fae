module Blockchain.Fae.Contracts 
  (
    verifySig 
  ) where

import Blockchain.Fae

verifySig :: a -> PublicKey -> EntryID -> Signature -> Fae a
verifySig x key self sig
  | key == signer sig self = return x
  | otherwise = throw "Bad signature"

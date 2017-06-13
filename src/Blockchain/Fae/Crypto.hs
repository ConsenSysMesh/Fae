module Blockchain.Fae.Crypto 
  (
    PublicKey, Signature,
    verifySig 
  ) where

import Blockchain.Fae.Internal.Crypto

verifySig :: (Digestible a) => PublicKey -> a -> Signature -> Bool
verifySig key x sig = key == signer sig x


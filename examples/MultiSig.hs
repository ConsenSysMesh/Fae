module MultiSig (Signatures(..), tokenContract) where

import Blockchain.Fae
import Blockchain.Fae.Contracts (sigContract)
import Blockchain.Fae.Crypto (verifySig)

import Data.Function
import Data.Monoid

data Token = Token

-- sample TX to create a tokenContract:
-- tx = do
--   self <- createMonoid (tokenContract key1 key2 keyR self)
--   return ()

data Signatures =
  Signatures
  {
    sig1 :: Maybe Signature,
    sig2 :: Maybe Signature
  }

instance Monoid Signatures where
  mempty = Signatures{sig1 = Nothing, sig2 = Nothing}
  mappend sigsNew sigsOld =
    Signatures
    {
      sig1 = (takeLatest `on` sig1) sigsNew sigsOld,
      sig2 = (takeLatest `on` sig2) sigsNew sigsOld
    }
    where
      takeLatest Nothing Nothing = Nothing
      takeLatest (Just xNew) Nothing = Just xNew
      takeLatest Nothing (Just xOld) = Just xOld
      -- This has a bit of a security hole in that someone can overwrite
      -- a valid signature.
      takeLatest (Just xNew) (Just xOld) = Just xNew 

-- When used as a contract, the `PublicKey`s need to be supplied at
-- creation; the signatures are provided in subsequent transactions.
-- The return value is empty because the contract provides nothing to the
-- signatories, but rather to the hard-coded third public key.
tokenContract :: 
  PublicKey -> PublicKey -> PublicKey -> EntryID -> 
  Signatures -> Fae ()
tokenContract key1 key2 keyR self Signatures{sig1 = Just s1, sig2 = Just s2} =
  | verifySig key1 self s1 && verifySig key2 self s2 = do
      cSelf <- createPure $ sigContract Token (escrow ()) keyR cSelf
      return ()
tokenContract _ _ _ _ _ _ = return Nothing


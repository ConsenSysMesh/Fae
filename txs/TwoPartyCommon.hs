module TwoPartyCommon where

import Blockchain.Fae
import Blockchain.Fae.Contracts
import Blockchain.Fae.Internal (ContractID, Digestible, public, newPrivateKey) 
import Crypto.Random
import Data.Maybe
import Data.Void

import System.IO.Unsafe

{-# NOINLINE pubKeys #-}
pubKeys :: (PublicKey, PublicKey)
pubKeys = fst $ withDRG drg $ do
  Just key1 <- public <$> newPrivateKey
  Just key2 <- public <$> newPrivateKey
  return (key1, key2)
  where
    drg = drgNewSeed $ seedFromInteger 0

pubKey1 :: PublicKey
pubKey1 = fst pubKeys

pubKey2 :: PublicKey
pubKey2 = snd pubKeys

offer2TX :: TwoParties -> Transaction Void ()
offer2TX p _ = offer2 p ("Hello from " ++ show p) 


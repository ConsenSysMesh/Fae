module Blockchain.Fae.Internal.Crypto where

data PublicKey = PublicKey deriving (Eq)
data Signature = Signature

class Digestible a where

signer :: Signature -> a -> PublicKey
signer = undefined

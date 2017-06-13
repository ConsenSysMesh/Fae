module Blockchain.Fae.Internal.Crypto where

data PublicKey = PublicKey deriving (Eq)
data Signature = Signature
data Digest = Digest deriving (Eq, Ord)

class Digestible a where
  digest :: a -> Digest
  digest _ = Digest

digestWith :: (Digestible a) => Digest -> a -> Digest
digestWith = undefined

signer :: Signature -> a -> PublicKey
signer = undefined

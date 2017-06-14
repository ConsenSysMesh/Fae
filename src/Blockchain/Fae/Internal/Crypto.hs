module Blockchain.Fae.Internal.Crypto where

import Data.Dynamic

data PublicKey = PublicKey deriving (Eq)
data Signature = Signature
data Digest = Digest deriving (Eq, Ord, Show)

class Digestible a where
  digest :: a -> Digest
  digest _ = Digest

instance Digestible Dynamic

digestWith :: (Digestible a) => Digest -> a -> Digest
digestWith = undefined

signer :: Signature -> a -> PublicKey
signer = undefined

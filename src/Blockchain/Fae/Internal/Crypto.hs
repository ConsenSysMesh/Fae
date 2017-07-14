module Blockchain.Fae.Internal.Crypto 
  (
    Serialize,
    module Blockchain.Fae.Internal.Crypto
  ) where

import qualified Crypto.Hash as Hash

import qualified Data.ByteArray as BA
import Data.Dynamic
import Data.Serialize (Serialize)

import qualified Data.Serialize as Ser
import qualified Data.Serialize.Put as Ser
import qualified Data.Serialize.Get as Ser

data PublicKey = PublicKey deriving (Eq)
data Signature = Signature
type Digest = Hash.Digest Hash.SHA3_256

class Digestible a where
  digest :: a -> Digest

  default digest :: (Serialize a) => a -> Digest
  digest = Hash.hash . Ser.encode

instance Serialize Digest where
  put = Ser.putByteString . BA.convert
  get = Ser.isolate hashSize $ do
    digestBS <- Ser.getBytes hashSize
    let Just result = Hash.digestFromByteString digestBS
    return result
    where hashSize = Hash.hashDigestSize Hash.SHA3_256

instance Digestible Digest
instance Digestible ()
instance Digestible Int

(<#>) :: (Serialize a, Serialize b) => a -> b -> Digest
d1 <#> d2 = 
  Hash.hashFinalize $ 
  Hash.hashUpdates Hash.hashInit $
  [Ser.encode d1, Ser.encode d2] 

signer :: Signature -> a -> PublicKey
signer = undefined

nullDigest :: Digest
nullDigest = Hash.hash (BA.empty :: BA.Bytes)

module Blockchain.Fae.Internal.Crypto 
  (
    Serialize,
    module Blockchain.Fae.Internal.Crypto
  ) where

import Control.DeepSeq
import Control.Monad

import Crypto.Error
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Random.Types

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Dynamic
import Data.Proxy
import Data.Serialize (Serialize)

import qualified Data.Serialize as Ser
import qualified Data.Serialize.Put as Ser
import qualified Data.Serialize.Get as Ser

import GHC.Generics

import System.IO.Unsafe

{- Types -}

-- These are only here to avoid an UndecidableInstance in the Serialize
-- instance.
newtype Crypto a = Crypto a deriving (Eq, Ord, NFData)
type EdPublicKey = Crypto Ed.PublicKey
type EdSecretKey = Crypto Ed.SecretKey
type EdSignature = Crypto Ed.Signature
type Digest = Crypto (Hash.Digest Hash.SHA3_256)

-- | It's what it looks like.  We don't implement our own crypto because we
-- aren't crazy; this is taken from 'cryptonite'.
type PublicKey = EdPublicKey
data PrivateKey = PrivateKey EdPublicKey EdSecretKey deriving (Generic)
data Signature = Signature EdPublicKey EdSignature deriving (Generic)

data Signed a = Signed {sig :: Signature, body :: a} deriving (Generic)

{- Type classes -}
class Digestible a where
  digest :: a -> Digest

  default digest :: (Serialize a) => a -> Digest
  digest = Crypto . Hash.hash . Ser.encode

class (Monad t) => PassFail t where
  passOrThrow :: t a -> a

class (PassFail (PassFailM a)) => PartialDecode a where
  type PassFailM a :: * -> *
  encode :: a -> BS.ByteString
  decodeSize :: Proxy a -> Int
  partialDecode :: BS.ByteString -> PassFailM a a

{- Instances -}
instance (Show a) => Show (Crypto a) where
  show (Crypto x) = show x

instance PassFail Maybe where
  passOrThrow (Just x) = x
  passOrThrow Nothing = error "PassFail failed with Nothing"

instance PassFail CryptoFailable where
  passOrThrow = throwCryptoError

instance PartialDecode (Hash.Digest Hash.SHA3_256) where
  type PassFailM (Hash.Digest Hash.SHA3_256) = Maybe
  encode = BA.convert
  decodeSize _ = Hash.hashDigestSize Hash.SHA3_256
  partialDecode = Hash.digestFromByteString

instance PartialDecode Ed.PublicKey where
  type PassFailM Ed.PublicKey = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.publicKeySize
  partialDecode = Ed.publicKey

instance PartialDecode Ed.SecretKey where
  type PassFailM Ed.SecretKey = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.secretKeySize
  partialDecode = Ed.secretKey

instance PartialDecode Ed.Signature where
  type PassFailM Ed.Signature = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.signatureSize
  partialDecode = Ed.signature

instance (PartialDecode a) => Serialize (Crypto a) where
  put (Crypto x) = Ser.putByteString $ encode x
  get = Ser.isolate numBytes $ do
    encodeBS <- Ser.getBytes numBytes
    return $ Crypto $ passOrThrow $ partialDecode encodeBS
    where numBytes = decodeSize $ Proxy @a

-- instance Serialize PublicKey
-- instance Serialize Digest
instance Serialize PrivateKey
instance Serialize Signature
instance (Serialize a) => Serialize (Signed a)

instance (PartialDecode a) => Digestible (Crypto a)

instance Digestible ()
instance Digestible Int
instance Digestible Char
instance Digestible Integer
instance (Serialize a, Serialize b) => Digestible (a, b)
instance (Serialize a) => Digestible [a]
instance (Serialize a) => Digestible (Maybe a) 
instance (Serialize a, Serialize b) => Digestible (Either a b)

instance Read Digest where
  readsPrec _ s = 
    case Ser.decode bs of
      Left _ -> []
      Right dig -> [(dig, C8.unpack rest)]
    where (bs, rest) = B16.decode $ C8.pack s

{- Functions -}

sign :: (Digestible a) => a -> PrivateKey -> Signed a
sign x (PrivateKey pubKey@(Crypto edPubKey) (Crypto secKey)) = 
  Signed
  {
    sig = Signature pubKey $ Crypto $ Ed.sign secKey edPubKey dig,
    body = x
  }
  where Crypto dig = digest x

unsign :: (Digestible a) => Signed a -> Maybe PublicKey
unsign Signed{sig = Signature pubKey@(Crypto edPubKey) (Crypto sig), body = msg}
  | Ed.verify edPubKey dig sig = Just pubKey
  | otherwise = Nothing
  where Crypto dig = digest msg

newPrivateKey :: (MonadRandom m) => m PrivateKey
newPrivateKey = do
  secKey <- Ed.generateSecretKey
  let pubKey = Ed.toPublic secKey
  return $ PrivateKey (Crypto pubKey) (Crypto secKey)

{-# NOINLINE unsafeNewPrivateKey #-}
unsafeNewPrivateKey :: PrivateKey
unsafeNewPrivateKey = unsafePerformIO newPrivateKey

public :: PrivateKey -> Maybe PublicKey
public (PrivateKey pubKey@(Crypto edPubKey) (Crypto secKey))
  | Ed.toPublic secKey == edPubKey = Just pubKey
  | otherwise = Nothing


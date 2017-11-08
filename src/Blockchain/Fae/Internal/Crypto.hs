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
import Data.Char
import Data.Dynamic
import Data.List
import Data.Proxy
import Data.Serialize (Serialize)
import Data.String

import qualified Data.Serialize as Ser
import qualified Data.Serialize.Put as Ser
import qualified Data.Serialize.Get as Ser

import GHC.Generics

import System.IO.Unsafe

{- Types -}

-- These are only here to avoid an UndecidableInstance in the Serialize
-- instance.
newtype EdPublicKey = EdPublicKey Ed.PublicKey deriving (Eq, NFData)
newtype EdSecretKey = EdSecretKey Ed.SecretKey deriving (Eq, NFData)
newtype EdSignature = EdSignature Ed.Signature deriving (Eq, NFData)
newtype Digest = HashDigest (Hash.Digest Hash.SHA3_256) deriving (Eq, Ord, NFData)

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
  digest = HashDigest . Hash.hash . Ser.encode

class (Monad t) => PassFail t where
  passOrThrow :: t a -> a

class (PassFail (PassFailM a)) => PartialDecode a where
  type PassFailM a :: * -> *
  encode :: a -> BS.ByteString
  decodeSize :: Proxy a -> Int
  partialDecode :: BS.ByteString -> PassFailM a a

{- Instances -}

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

instance Serialize EdPublicKey where
  put (EdPublicKey x) = putPartialDecode x
  get = EdPublicKey <$> getPartialDecode

instance Serialize EdSecretKey where
  put (EdSecretKey x) = putPartialDecode x
  get = EdSecretKey <$> getPartialDecode

instance Serialize EdSignature where
  put (EdSignature x) = putPartialDecode x
  get = EdSignature <$> getPartialDecode

instance Serialize Digest where
  put (HashDigest x) = putPartialDecode x
  get = HashDigest <$> getPartialDecode

-- instance Serialize PublicKey
-- instance Serialize Digest
instance Serialize PrivateKey
instance Serialize Signature
instance (Serialize a) => Serialize (Signed a)

instance Digestible EdPublicKey
instance Digestible EdSecretKey
instance Digestible EdSignature
instance Digestible Digest

instance Digestible ()
instance Digestible Int
instance Digestible Char
instance Digestible Integer
instance (Serialize a, Serialize b) => Digestible (a, b)
instance (Serialize a) => Digestible [a]
instance (Serialize a) => Digestible (Maybe a) 
instance (Serialize a, Serialize b) => Digestible (Either a b)

instance Read Digest where
  readsPrec = readsPrecSer

instance Read EdPublicKey where
  readsPrec = readsPrecSer

instance IsString Digest where
  fromString = read

instance IsString EdPublicKey where
  fromString = read

instance Show EdPublicKey where
  show = C8.unpack . B16.encode . Ser.encode

instance Show Digest where
  show (HashDigest x) = show x

{- Functions -}

putPartialDecode :: (PartialDecode a) => a -> Ser.Put
putPartialDecode x = Ser.putByteString $ encode x

getPartialDecode :: forall a. (PartialDecode a) => Ser.Get a
getPartialDecode = Ser.isolate numBytes $ do
  encodeBS <- Ser.getBytes numBytes
  return $ passOrThrow $ partialDecode encodeBS
  where numBytes = decodeSize $ Proxy @a

readsPrecSer :: (Serialize a) => Int -> String -> [(a, String)]
readsPrecSer _ s = 
  case Ser.decode bs of
    Left _ -> []
    Right x -> [(x, C8.unpack rest)]
  where (bs, rest) = B16.decode $ C8.pack $ dropWhile isSpace s

sign :: (Digestible a) => a -> PrivateKey -> Signed a
sign x (PrivateKey pubKey@(EdPublicKey edPublicKey) (EdSecretKey secKey)) = 
  Signed
  {
    sig = Signature pubKey $ EdSignature $ Ed.sign secKey edPublicKey dig,
    body = x
  }
  where HashDigest dig = digest x

unsign :: (Digestible a) => Signed a -> Maybe PublicKey
unsign Signed{sig = Signature pubKey@(EdPublicKey edPublicKey) (EdSignature sig), body = msg}
  | Ed.verify edPublicKey dig sig = Just pubKey
  | otherwise = Nothing
  where HashDigest dig = digest msg

newPrivateKey :: (MonadRandom m) => m PrivateKey
newPrivateKey = do
  secKey <- Ed.generateSecretKey
  let pubKey = Ed.toPublic secKey
  return $ PrivateKey (EdPublicKey pubKey) (EdSecretKey secKey)

{-# NOINLINE unsafeNewPrivateKey #-}
unsafeNewPrivateKey :: PrivateKey
unsafeNewPrivateKey = unsafePerformIO newPrivateKey

public :: PrivateKey -> Maybe PublicKey
public (PrivateKey pubKey@(EdPublicKey edPublicKey) (EdSecretKey secKey))
  | Ed.toPublic secKey == edPublicKey = Just pubKey
  | otherwise = Nothing


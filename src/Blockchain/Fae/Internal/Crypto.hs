{- |
Module: Blockchain.Fae.Internal.Crypto
Description: Cryptography for Fae
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module extends the types of "Crypto.Hash" and "Crypto.PubKey.Ed25519" from @cryptonite@ to be easily usable for Fae, and defines some convenience functions for them.
-}
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
import Data.Ord
import Data.Proxy
import Data.Serialize (Serialize)
import Data.String

import qualified Data.Serialize as Ser
import qualified Data.Serialize.Put as Ser
import qualified Data.Serialize.Get as Ser

import GHC.Generics

import Numeric.Natural

import System.IO.Unsafe

-- * Types

-- These are only here to avoid an UndecidableInstance in the Serialize
-- instance.

-- | Newtyped 'Ed.PublicKey' for defining my own instances of things.
newtype EdPublicKey = EdPublicKey Ed.PublicKey deriving (Eq, NFData)
-- | Newtyped 'Ed.SecretKey' for defining my own instances of things.
newtype EdSecretKey = EdSecretKey Ed.SecretKey deriving (Eq, NFData)
-- | Newtyped 'Ed.Signature' for defining my own instances of things.
newtype EdSignature = EdSignature Ed.Signature deriving (Eq, NFData)
-- | Newtyped 'Hash.Digest' 'Hash.SHA3_256' for defining my own instances
-- of things.
newtype Digest = HashDigest (Hash.Digest Hash.SHA3_256) deriving (Eq, Ord, NFData)

-- | It's what it looks like.  We don't implement our own crypto because we
-- aren't crazy; this is taken from 'cryptonite'.
type PublicKey = EdPublicKey
-- | We include both the public and private keys here because the Ed25519
-- signing function needs both of them, and that's just semantically silly.
data PrivateKey = PrivateKey EdPublicKey EdSecretKey deriving (Generic)
-- | We include the public key along with the signature because there
-- appears to be no public-key recovery function from Ed25519 signatures.
data Signature = Signature EdPublicKey EdSignature deriving (Generic, Eq)

-- | A useful abstraction, again allowing semantic improvements in 'sign'.
data Signed a = Signed {unSigned :: a, sig :: Signature} deriving (Generic)

-- * Type classes
-- | This is probably a duplicate of some @Hashable@ class, but I want
-- total control over the hashing algorithm and method implementations.
-- Since 'digest' has a default, one should never have to actaully
-- implement this class.
class Digestible a where
  -- | Hashes any 'Digestible'.
  -- The default digest is the hash of the binary serialization.
  digest :: a -> Digest

  default digest :: (Serialize a) => a -> Digest
  digest = HashDigest . Hash.hash . Ser.encode

-- | Class #1 for uniformly defining 'Serialize' instances for all the
-- crypto types above.
class (Monad t) => PassFail t where
  -- | Essentially, 'passOrThrow' is like @fromMaybe (error err)@, but for
  -- any monad 't' instead of just 'Maybe'.
  passOrThrow :: t a -> a

-- | Class #2 for uniformly defining 'Serialize' instances for all the
-- crypto types above.
class (PassFail (PassFailM a)) => PartialSerialize a where
  -- | The container type for the result of decoding, or the error from
  -- doing so.
  type PassFailM a :: * -> *
  -- | A binary serialization for 'a'; no errors are possible here.
  encode :: a -> BS.ByteString
  -- | The required number of bytes for decoding a bytestring.
  decodeSize :: Proxy a -> Int
  -- | Decodes a bytestring.  The reason it's partial, of course, is that
  -- it may include an error.  Although I prefer @cereal@ to @binary@
  -- because its decode function returns a sum type instead of throwing,
  -- I choose to use this flexibility to throw anyway.
  partialDecode :: BS.ByteString -> PassFailM a a

{- Instances -}

-- | Decoding a hash returns a 'Maybe' value.  This is of course the
-- motivating type for the 'PassFail' class.
instance PassFail Maybe where
  passOrThrow (Just x) = x
  passOrThrow Nothing = error "PassFail failed with Nothing"

-- | Decoding any of the cryptographic types returns 'CryptoFailable',
-- which has a number of different types of error but still only one
-- decoded value.
instance PassFail CryptoFailable where
  passOrThrow = throwCryptoError

-- Only @cryptonite-0.24@ and up export 'hashDigestSize', which we use for
-- 'decodeSize'.
-- | 'partialDecode' is 'digestFromByteString'.  
instance PartialSerialize (Hash.Digest Hash.SHA3_256) where
  type PassFailM (Hash.Digest Hash.SHA3_256) = Maybe
  encode = BA.convert
  decodeSize _ = Hash.hashDigestSize Hash.SHA3_256
  partialDecode = Hash.digestFromByteString

-- 'publicKeySize' comes from @cryptonite-0.24@.
-- | 'partialDecode' is 'publicKey', which is very finicky and should
-- probably just be a 'Serialize' instance for 'Ed.PublicKey'.
instance PartialSerialize Ed.PublicKey where
  type PassFailM Ed.PublicKey = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.publicKeySize
  partialDecode = Ed.publicKey

-- 'secretKeySize' comes from @cryptonite-0.24@.
-- | 'partialDecode' is 'secretKey', which is very finicky and should
-- probably just be a 'Serialize' instance for 'Ed.SecretKey'.
instance PartialSerialize Ed.SecretKey where
  type PassFailM Ed.SecretKey = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.secretKeySize
  partialDecode = Ed.secretKey

-- 'signatureSize' comes from @cryptonite-0.24@.
-- | 'partialDecode' is 'signature', which is very finicky and should
-- probably just be a 'Serialize' instance for 'Ed.Signature'.
instance PartialSerialize Ed.Signature where
  type PassFailM Ed.Signature = CryptoFailable
  encode = BA.convert
  decodeSize _ = Ed.signatureSize
  partialDecode = Ed.signature

-- | Uniform serialization via 'PartialSerialize'
instance Serialize EdPublicKey where
  put (EdPublicKey x) = putPartialSerialize x
  get = EdPublicKey <$> getPartialSerialize

-- | Uniform serialization via 'PartialSerialize'
instance Serialize EdSecretKey where
  put (EdSecretKey x) = putPartialSerialize x
  get = EdSecretKey <$> getPartialSerialize

-- | Uniform serialization via 'PartialSerialize'
instance Serialize EdSignature where
  put (EdSignature x) = putPartialSerialize x
  get = EdSignature <$> getPartialSerialize

-- | Uniform serialization via 'PartialSerialize'
instance Serialize Digest where
  put (HashDigest x) = putPartialSerialize x
  get = HashDigest <$> getPartialSerialize

-- instance Serialize PublicKey
-- instance Serialize Digest
-- | -
instance Serialize PrivateKey
-- | -
instance Serialize Signature
-- | -
instance (Serialize a) => Serialize (Signed a)

-- | -
instance Digestible EdPublicKey
-- | -
instance Digestible EdSecretKey
-- | -
instance Digestible EdSignature
-- | -
instance Digestible Digest

-- | -
instance Digestible ()
-- | -
instance Digestible Int
-- | -
instance Digestible Word
-- | -
instance Digestible Char
-- | -
instance Digestible Integer
-- | -
instance Digestible Float
-- | -
instance Digestible Double
-- | -
instance Digestible Natural
-- | -
instance Digestible BS.ByteString
-- | -
instance (Serialize a, Serialize b) => Digestible (a, b)
-- | -
instance (Serialize a) => Digestible [a]
-- | -
instance (Serialize a) => Digestible (Maybe a) 
-- | -
instance (Serialize a, Serialize b) => Digestible (Either a b)
-- | -
instance (Serialize a, Serialize b, Serialize c) => Digestible (a, b, c)

-- | Uniform instance from 'Serialize'
instance Read Digest where
  readsPrec = readsPrecSer

-- | Uniform instance from 'Serialize'
instance Read EdPublicKey where
  readsPrec = readsPrecSer

-- | Uniform instance from 'Serialize'
instance Ord EdPublicKey where
  compare = compareSerialize

-- | Uniform instance from 'Serialize'
instance Ord EdSecretKey where
  compare = compareSerialize

-- | Uniform instance from 'Serialize'
instance Ord EdSignature where
  compare = compareSerialize

-- | We want an 'IsString' instance for the convenience of transaction
-- authors, who have to provide a contract ID that is, ultimately,
-- a 'Digest'.
instance IsString Digest where
  fromString = read

-- | Contracts may need to contain public keys that come from outside Fae,
-- and it should be easy to enter them.
instance IsString EdPublicKey where
  fromString = read

-- | Public keys 'show' as hex strings.
instance Show EdPublicKey where
  show = printHex

-- | Digests 'show' as hex strings.
instance Show Digest where
  show (HashDigest x) = show x

-- * Functions

-- | Just compare the bytestrings lexicographically
compareSerialize :: (Serialize a) => a -> a -> Ordering
compareSerialize a b = compare (Ser.encode a) (Ser.encode b)

-- | 'put' for 'Serialize' is easy to define: just convert to a bytestring,
-- which is what 'PartialSerialize' allows us to do.
putPartialSerialize :: (PartialSerialize a) => a -> Ser.Put
putPartialSerialize x = Ser.putByteString $ encode x

-- | 'get' for 'Serialize' is not so simple.  You have to make sure you are
-- consuming exactly the right number of bytes, with none left over, before
-- decoding.  Fortunately 'isolate' does this nicely.
getPartialSerialize :: forall a. (PartialSerialize a) => Ser.Get a
getPartialSerialize = Ser.isolate numBytes $ do
  encodeBS <- Ser.getBytes numBytes
  return $ passOrThrow $ partialDecode encodeBS
  where numBytes = decodeSize $ Proxy @a

-- | This allows us to 'read' any serializable type from a hex-string
-- representation of its binary serialization.
readsPrecSer :: (Serialize a) => Int -> String -> [(a, String)]
readsPrecSer _ s = 
  case Ser.decode bs of
    Left _ -> []
    Right x -> [(x, C8.unpack rest)]
  where (bs, rest) = B16.decode $ C8.pack $ dropWhile isSpace s

-- | This function replaces 'Ed.sign' to use our semantically-nice types.
--
-- prop> forall x. (Digestible x) => unsign . sign x = public
sign :: (Digestible a) => a -> PrivateKey -> Signed a
sign x (PrivateKey pubKey@(EdPublicKey edPublicKey) (EdSecretKey secKey)) = 
  Signed
  {
    sig = Signature pubKey $ EdSignature $ Ed.sign secKey edPublicKey dig,
    unSigned = x
  }
  where HashDigest dig = digest x

-- | The public-key signature recovery function.  Doubles as a way to
-- verify a signature.  Satisfies the law
--
-- prop> forall x. (Digestible x) => unsign . sign x = public
unsign :: (Digestible a) => Signed a -> Maybe PublicKey
unsign Signed{sig = Signature pubKey@(EdPublicKey edPublicKey) (EdSignature sig), unSigned = msg}
  | Ed.verify edPublicKey dig sig = Just pubKey
  | otherwise = Nothing
  where HashDigest dig = digest msg

-- | When we need the public key without validating the signature
getSigner :: Signature -> PublicKey
getSigner (Signature pubKey _) = pubKey

-- | Randomly creates a new private key using 'generateSecretKey', which
-- itself relies on 'MonadRandom'.  Fortunately @cryptonite@ handles the
-- intricacies of choosing the random value appropriately.
newPrivateKey :: (MonadRandom m) => m PrivateKey
newPrivateKey = do
  secKey <- Ed.generateSecretKey
  let pubKey = Ed.toPublic secKey
  return $ PrivateKey (EdPublicKey pubKey) (EdSecretKey secKey)

{-# NOINLINE unsafeNewPrivateKey #-}
-- | This very evil function should only be used if you want a single
-- ad-hoc private key.  Using it to define two or more keys will not work
-- as you expect!
unsafeNewPrivateKey :: PrivateKey
unsafeNewPrivateKey = unsafePerformIO newPrivateKey

-- | Converts a private key into its corresponding public key, and at the
-- same time, verifies the 'PrivateKey' value.  Satisfies the law
--
-- prop> unsign . sign = public
public :: PrivateKey -> Maybe PublicKey
public (PrivateKey pubKey@(EdPublicKey edPublicKey) (EdSecretKey secKey))
  | Ed.toPublic secKey == edPublicKey = Just pubKey
  | otherwise = Nothing

-- | Useful function inexplicably missing from every library
printHex :: (Serialize a) => a -> String
printHex = C8.unpack . B16.encode . Ser.encode

printShortHex :: (Serialize a) => a -> String
printShortHex = (++ "...") . C8.unpack . C8.take 4 . B16.encode . Ser.encode

-- | The default for various things
nullDigest :: Digest
nullDigest = digest ()


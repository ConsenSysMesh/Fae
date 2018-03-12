{-# LANGUAGE UndecidableInstances #-}
{- |
Module: Blockchain.Fae.Internal.NFData
Description: Automatic Generic instance of NFData
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

For the convenience of contract authors, this module provides an automatic
instance for the 'NFData' class from 'Generic'.  It also serves the purpose
of guaranteeing that @rnf x@ actually means "fully evaluate @x@", which is
not actually necessarily true of an instance that someone malicious might
write.
-}
module Blockchain.Fae.Internal.NFData where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Versions

import qualified Control.DeepSeq as DS

import Data.Foldable

import GHC.Generics

import Numeric.Natural

-- | So we can have control over what is an instance and how.
newtype RNF a = RNF {getRNF :: a}

-- | For writing constraints easily.
type NFData a = DS.NFData (RNF a)

-- | Alternative to "Control.DeepSeq"'s private class of the same name.
class GNFData f where
  grnf :: f p -> ()

-- | Replacement for 'Control.DeepSeq.rnf'
rnf :: (NFData a) => a -> ()
rnf = DS.rnf . RNF 

-- | Replacement for 'Control.DeepSeq.force'
force :: (NFData a) => a -> a
force = getRNF . DS.force . RNF 

-- | Replacement for 'Control.DeepSeq.deepseq'
deepseq :: (NFData a) => a -> b -> b
deepseq = DS.deepseq . RNF 

infixr 0 $!!
-- | Replacement for 'Control.DeepSeq.$!!'
($!!) :: (NFData a) => (a -> b) -> a -> b
($!!) f x = (f . getRNF) DS.$!! RNF  x

infixl 4 <$!!>
-- | Replacement for 'Control.DeepSeq.<$!!>'
(<$!!>) :: (NFData b, Monad m) => (a -> b) -> m a -> m b
(<$!!>) f mx = fmap getRNF $ (RNF  . f) DS.<$!!> mx

-- | -
deriving instance DS.NFData (RNF Word)
-- | -
deriving instance DS.NFData (RNF Char)
-- | -
deriving instance DS.NFData (RNF Int)
-- | -
deriving instance DS.NFData (RNF Integer)
-- | -
deriving instance DS.NFData (RNF Float)
-- | -
deriving instance DS.NFData (RNF Double)
-- | -
deriving instance DS.NFData (RNF Natural)
-- | -
deriving instance DS.NFData (RNF Digest)
-- | -
deriving instance DS.NFData (RNF PublicKey)
-- | -
deriving instance DS.NFData (RNF EdSecretKey)
-- | -
deriving instance DS.NFData (RNF EdSignature)
-- | -
deriving instance DS.NFData (RNF (EscrowID argType valType))

-- | Seems reasonable.  This presents an annoyance for user-defined
-- parameterized types that are /not/ 'Foldable'.
instance {-# OVERLAPPABLE #-}
  (NFData a, Foldable t) => DS.NFData (RNF (t a)) where
    
  rnf (RNF xs) = foldr' (\x !s -> rnf x) () xs

-- | -
instance GNFData V1 where
  grnf !_ = error "Blockchain.Fae.Internal.NFData.rnf: uninhabited type"

-- | -
instance GNFData U1 where
  grnf U1 = ()

-- | -
instance (GNFData f, GNFData g) => GNFData (f :+: g) where
  grnf (L1 x) = grnf x
  grnf (R1 x) = grnf x

-- | -
instance (GNFData f, GNFData g) => GNFData (f :*: g) where
  grnf (x :*: y) = grnf x `seq` grnf y

-- | -
instance (DS.NFData (RNF c)) => GNFData (K1 i c) where
  grnf (K1 x) = DS.rnf (RNF  x)

-- | -
instance (GNFData f) => GNFData (M1 i t f) where
  grnf (M1 x) = grnf x


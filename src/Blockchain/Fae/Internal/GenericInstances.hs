{-# LANGUAGE UndecidableInstances #-}
{- |
Module: Blockchain.Fae.Internal.GenericInstances
Description: Automatic instances for generic types
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental
-}
module Blockchain.Fae.Internal.GenericInstances where

import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.NFData
import Blockchain.Fae.Internal.Versions

import qualified Control.DeepSeq as DS
import Control.Monad.State

import Data.Maybe
import Data.Typeable

import GHC.Generics

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Generic a, GHasEscrowIDs (Rep a)) => HasEscrowIDs a where

  traverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (
    Typeable a, Generic a, 
    GNFData (Rep a), GHasEscrowIDs (Rep a), GGetInputValues (Rep a)
  ) => 
  GetInputValues a where

  getInputValues l = fromMaybe 
    (defaultGetInputValues l) 
    (runStateT (to <$> gGetInputValues) l)

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (HasEscrowIDs a, Typeable a, Generic a, GVersionable (Rep a)) => 
  Versionable a where

  mapVersions vMap = to . gMapVersions vMap . from 
  versions f = gVersions f . from

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-} 
  (Generic a, GNFData (Rep a)) => DS.NFData (RNF a) where

  rnf (RNF x) = grnf (from x)



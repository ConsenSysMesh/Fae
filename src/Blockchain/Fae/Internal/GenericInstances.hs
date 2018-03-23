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

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.Versions

import Control.Monad.State

import Data.Maybe
import Data.Typeable

import GHC.Generics

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Generic a, Typeable a, GHasEscrowIDs (Rep a)) => HasEscrowIDs a where

  traverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Typeable a, Generic a, GHasEscrowIDs (Rep a), GGetInputValues (Rep a)) => 
  GetInputValues a where

  getInputValues l = check $ fromMaybe 
    (defaultGetInputValues l) 
    (runStateT (to <$> gGetInputValues) l)
    where check ~(x, unused) = if null unused then x else throw TooManyInputs

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (HasEscrowIDs a, Typeable a, Generic a, GVersionable (Rep a)) => 
  Versionable a where

  mapVersions vMap = to . gMapVersions vMap . from 
  versions f = gVersions f . from

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

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Serialization

import Control.Monad.State

import Data.Maybe
import Data.Typeable

import Data.Serialize (Serialize, GSerializePut, GSerializeGet)
import qualified Data.Serialize as S

import GHC.Generics

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Generic a, Typeable a, GHasEscrowIDs (Rep a)) => HasEscrowIDs a where

  traverseEscrowIDs f x = to <$> gTraverseEscrowIDs f (from x)

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Typeable a, EGeneric a, Serialize (ERep a)) => Exportable a where

  exportValue = fmap S.encode . eFrom
  importValue = either (const $ return Nothing) (fmap Just . eTo) . S.decode

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-}
  (Generic a, EGeneric1 (Rep a), ERep a ~ SERep1 (Rep a)) => EGeneric a where

  eFrom = fmap SERep1 . eFrom1 @_ @_ @() . from 
  eTo (SERep1 x) = to <$> eTo1 @_ @_ @() x

-- | /So/ undecidable
instance {-# OVERLAPPABLE #-} (Serialize (ERep a)) => ESerialize a


{- |
Module: Blockchain.Fae.Internal.GetInputValues
Description: Constructor for transaction inputs
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental
-}
module Blockchain.Fae.Internal.GetInputValues where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.NFData
import Blockchain.Fae.Internal.Versions

import Control.Applicative
import Control.Monad.State

import Data.Typeable

import GHC.Generics

import Numeric.Natural

-- | This class controls how a type is constructed from
-- a heterogeneously-typed list of input return values.  Its default (and,
-- through the magic of 'UndefinedInstances', automatic) instance, for any
-- 'Generic' type, simply assigns each field of a product type from
-- successive values in the list and sum types as-is from a single value,
-- failing if they don't all match or there are extras on one side.  For
-- the moment, the member function is not exported, so you can't write your
-- own implementations.
class (NFData a) => GetInputValues a where
  getInputValues :: [BearsValue] -> (a, [BearsValue])
  default 
    getInputValues :: 
      (HasEscrowIDs a, Typeable a) => [BearsValue] -> (a, [BearsValue])
  getInputValues = defaultGetInputValues

-- | Generic helper class
class GGetInputValues f where
  -- | This is in the 'State' monad, rather than having the same signature as
  -- 'getInputValues', because we need to walk through the list of dynamic
  -- inputs and progressively remove values from it.  At the same time, we
  -- need to know if there were leftovers.
  gGetInputValues :: StateT [BearsValue] Maybe (f p)

-- | Gets a single value from a single input
defaultGetInputValues :: 
  (HasEscrowIDs a, Typeable a) => [BearsValue] -> (a, [BearsValue])
defaultGetInputValues [] = (throw NotEnoughInputs, [])
defaultGetInputValues (xDyn : rest) = (x, rest) where
  x = unBear xDyn $ throw $ BadArgType (bearerType xDyn) (typeOf x) 

{- Instances -}

-- | Instance for non-Generic type
instance GetInputValues Char
-- | Instance for non-Generic type
instance GetInputValues Word
-- | Instance for non-Generic type
instance GetInputValues Int
-- | Instance for non-Generic type
instance GetInputValues Integer
-- | Instance for non-Generic type
instance GetInputValues Float
-- | Instance for non-Generic type
instance GetInputValues Double
-- | Instance for non-Generic type
instance GetInputValues Natural
-- | Instance for non-Generic type
instance GetInputValues PublicKey
-- | Don't want the Generic instance
instance 
  (
    HasEscrowIDs argType, HasEscrowIDs valType,
    Typeable argType, Typeable valType
  ) => 
  GetInputValues (EscrowID argType valType)

-- | Takes no values, but absorbs the entire list.  Useful for transactions
-- that don't use their input contracts.
instance GGetInputValues V1 where
  gGetInputValues = do
    put []
    return $ throw TooManyInputs

-- | We only allow product types to have input values, because how would
-- one choose between different branches of a sum type?  Non-algebraic
-- types are out of luck because we don't expose the methods of
-- 'GetInputValues' to be implemented manually.
instance (GGetInputValues f, GGetInputValues g) => GGetInputValues (f :*: g) where
  gGetInputValues = do
    l <- gGetInputValues
    r <- gGetInputValues
    return $ l :*: r

-- | For a nested type, we peel off one input and fix its type.  We do
-- /not/ do a recursive call.
instance (HasEscrowIDs c, Typeable c) => GGetInputValues (K1 i c) where
  gGetInputValues = do
    s <- gets null
    if s
    -- This allows you to use only part of an input structure.
    then return $ K1 $ throw NotEnoughInputs
    else K1 <$> state defaultGetInputValues

-- | Just ignore metadata for this one.
instance (GGetInputValues f) => GGetInputValues (M1 i t f) where
  gGetInputValues = M1 <$> gGetInputValues

-- | Abort: the "empty sum" type gets read as the whole type
instance GGetInputValues U1 where
  gGetInputValues = empty

-- | Abort: sum types get read as the whole type
instance GGetInputValues (f :+: g) where
  gGetInputValues = empty


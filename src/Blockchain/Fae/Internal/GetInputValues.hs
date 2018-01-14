{- |
Module: Blockchain.Fae.Internal.GetInputValues
Description: Constructor for transaction inputs
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental
-}
module Blockchain.Fae.Internal.GetInputValues where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Versions

import Control.Applicative
import Control.Monad.State

import Data.Typeable

import GHC.Generics hiding (to)
import qualified GHC.Generics as Gen (to)

import Numeric.Natural

-- | This class controls how a type is constructed from
-- a heterogeneously-typed list of input return values.  Its default
-- instance, for any 'Generic' type, simply assigns each field of a product
-- type from successive values in the list, failing if they don't all match
-- or there are extras on one side.  For the moment, the member function is
-- not exported, so you can't write your own implementations; however, you
-- do need to @instance GetInputValues a@ for any 'a' you choose
-- to use.
class GetInputValues a where
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
  forall a. (HasEscrowIDs a, Typeable a) => [BearsValue] -> (a, [BearsValue])
defaultGetInputValues (xDyn : rest) = (x, rest) where
  x = unBear xDyn $ throw $ BadArgType (bearerType xDyn) (typeOf x) 
defaultGetInputValues [] = throw NotEnoughInputs

{- Instances -}

-- | Instance for non-Generic type
instance GetInputValues Char
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
-- | Treats 'Versioned' like a newtype even though it's not.
instance (GetInputValues a) => GetInputValues (Versioned a) where
  getInputValues l = (Versioned x, l') where
    (x, l') = getInputValues l

-- | Has no values; uses one value if available, so that 'Void' can be used
-- as a black hole.
instance GGetInputValues V1 where
  gGetInputValues = do
    modify safeTail
    return $ throw TooManyInputs

    where
      safeTail [] = []
      safeTail (x : rest) = rest

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
    else fmap K1 $ state $ defaultGetInputValues

-- | Just ignore metadata for this one.
instance (GGetInputValues f) => GGetInputValues (M1 i t f) where
  gGetInputValues = M1 <$> gGetInputValues

instance GGetInputValues U1 where
  gGetInputValues = empty

instance GGetInputValues (f :+: g) where
  gGetInputValues = empty


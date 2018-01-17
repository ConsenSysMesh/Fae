{- |
Module: Blockchain.Fae.Internal.Lens
Description: Wrapper library for "Control.Lens"
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module just re-exports "Control.Lens", replacing 'makeLenses' with one that works exactly oppositely to the default one regarding underscored names, and including a utility function.
-}
module Blockchain.Fae.Internal.Lens 
  (
    module Blockchain.Fae.Internal.Lens,
    module Control.Lens
  ) where

import Control.Lens hiding (makeLenses)

import Data.Maybe

import Language.Haskell.TH

-- | Defined as
--
-- > makeLensesWith myLensRules
--
-- where 'myLensRules' /adds/ an underscore to names, rather than removes
-- one that exists.  Because the default behavior is backwards.
makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith myLensRules
  where 
    myLensRules = lensRules & lensField .~ myFieldNamer
    -- The inverse of the default field namer: ignores _-prefixed fields,
    -- otherwise prepends a _.
    myFieldNamer _ _ fName =
      case nameBase fName of
        ('_' : _) -> []
        x -> [TopName $ mkName $ '_' : x]

-- | Useful when using 'at', say
-- 
-- > someMapLens . at index . defaultLens (error $ show index ++ " not found!")
--
-- I'm pretty sure this can be replaced with some 'Traversal'-related
-- function, but I am not familiar enough with @lens@ yet to figure out
-- how.
defaultLens :: a -> Lens' (Maybe a) a
defaultLens x = lens (fromMaybe x) (flip $ const . Just)


module Blockchain.Fae.Internal.Lens 
  (
    module Blockchain.Fae.Internal.Lens,
    module Control.Lens
  ) where

import Control.Lens hiding (makeLenses)

import Language.Haskell.TH

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


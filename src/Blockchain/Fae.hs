module Blockchain.Fae where

import Blockchain.Fae.Internal

import Data.Monoid

createPure :: (a -> b) -> Fae ()
createPure f = create f const undefined

createMonoid :: (Monoid a) => (a -> b) -> Fae ()
createMonoid f = create f mappend mempty

module Blockchain.Fae 
  (
    Fae,
    EntryID, FacetID, EscrowID, Fee, Output, TransactionID,
    create, spend, evaluate,
    escrow, peek, close,
    facet, signer, label, follow
  ) where

import Blockchain.Fae.Internal

import Data.Monoid
import Data.Typeable

createPure :: (Typeable a, Typeable b) => (a -> Fae b) -> Fae EntryID
createPure f = create f const undefined

createMonoid :: (Monoid a, Typeable a, Typeable b) => (a -> Fae b) -> Fae EntryID
createMonoid f = create f mappend mempty

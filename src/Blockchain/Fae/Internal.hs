module Blockchain.Fae.Internal 
  (
    module Blockchain.Fae.Internal,
    module Blockchain.Fae.Internal.Types
  ) where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad.State

import Data.Dynamic
import Data.Maybe
import Data.Proxy
import Data.Void

create :: 
  (Typeable argT, Typeable accumT, Typeable valT) =>
  (accumT -> Fae valT) -> (argT -> accumT -> accumT) -> accumT -> Fae EntryID
create f c a = Fae $ do
  oldHash <- use $ _transientState . _lastHashUpdate
  facet <- use $ _transientState . _currentFacet
  let 
    newEntry = Entry (toDyn f) (toDyn c) (toDyn a) facet
    newHash = digestWith oldHash newEntry
    newEntryID = EntryID newHash
  _transientState . _lastHashUpdate .= newHash
  _transientState . _entryUpdates . _useEntries . at newEntryID ?= newEntry
  return newEntryID

evaluate ::
  forall argT valT.
  (Typeable argT, Typeable valT) =>
  EntryID -> argT -> Fae valT
evaluate entryID arg = Fae $ do
  entryM <- use $ _transientState . _entryUpdates . _useEntries . at entryID
  entry <- maybe
    (throwIO $ EvaluatedBadEntryID entryID)
    return
    entryM
  curFacet <- use $ _transientState . _currentFacet
  when (facet entry /= curFacet) $
    throwIO $ EvaluatedInWrongFacet entryID curFacet (facet entry)
  accumTransD <- maybe
    (throwIO $ 
      BadEntryArgType entryID (typeRep $ Proxy @argT) $
        head $ typeRepArgs $ dynTypeRep (combine entry)
    )
    return $
    dynApply (combine entry) (toDyn arg)
  let 
    -- The 'create' constructor guarantees that these two dynamic
    -- evaluations always type-check.
    newAccum = dynApp accumTransD (accum entry)
    faeValD = dynApp (contract entry) newAccum
  -- The accum update must go before the evaluation, because of the
  -- possibility of nested evaluations.
  _transientState . _entryUpdates . _useEntries . at entryID ?= 
    entry{accum = newAccum}
  faeVal <- fromDyn faeValD $
    throwIO $
      BadEntryValType entryID (typeRep $ Proxy @valT) $
        head $ typeRepArgs $ last $ typeRepArgs $ dynTypeRep $ contract entry
  getFae faeVal 


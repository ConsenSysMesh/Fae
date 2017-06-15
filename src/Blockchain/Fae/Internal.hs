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

spend :: a -> Fae a
spend x = Fae $ do
  entryID <- use $ _transientState . _currentEntry
  _transientState . _entryUpdates . _useEntries . at entryID .= Nothing
  return x

evaluate ::
  forall argT valT.
  (Typeable argT, Typeable valT) =>
  EntryID -> argT -> Fae valT
evaluate entryID arg = Fae $ do
  entryM <- use $ _transientState . _entryUpdates . _useEntries . at entryID
  entry <- maybe
    (throwIO $ BadEntryID entryID)
    return
    entryM
  curFacet <- use $ _transientState . _currentFacet
  when (facet entry /= curFacet) $
    throwIO $ WrongFacet entryID curFacet (facet entry)
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
  _transientState . _currentEntry .= entryID
  faeVal <- fromDyn faeValD $
    throwIO $
      BadEntryValType entryID (typeRep $ Proxy @valT) $
        head $ typeRepArgs $ last $ typeRepArgs $ dynTypeRep $ contract entry
  getFae faeVal 

escrow ::
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  tokT -> (privT -> pubT) -> privT -> 
  Fae (PublicEscrowID privT, PrivateEscrowID privT)
escrow tok pubF priv = Fae $ do
  oldHash <- use $ _transientState . _lastHashUpdate
  let
    escrow = Escrow (toDyn priv) (toDyn $ pubF priv) (toDyn tok)
    newHash = digestWith oldHash escrow
    escrowID = EscrowID newHash
  _transientState . _lastHashUpdate .= newHash
  _transientState . _escrows . _useEscrows . at escrowID ?= escrow
  return (PublicEscrowID escrowID, PrivateEscrowID escrowID)

peek ::
  forall privT pubT.
  (Typeable privT, Typeable pubT) =>
  PublicEscrowID privT -> Fae pubT
peek pubID = Fae $ do
  let PublicEscrowID escrowID = pubID
  escrowM <- use $ _transientState . _escrows . _useEscrows . at escrowID
  escrow <- maybe
    (throwIO $ BadEscrowID escrowID)
    return
    escrowM
  maybe
    (throwIO $ 
      BadPublicType escrowID (typeRep $ Proxy @pubT) (dynTypeRep $ public escrow))
    return $ 
    fromDynamic (public escrow)

close ::
  forall tokT privT.
  (Typeable tokT, Typeable privT) =>
  PrivateEscrowID privT -> tokT -> Fae privT
-- Need strictness here to force the caller to actually have a token, and not
-- just a typed 'undefined'.  The token itself is unused; we just need to
-- know that the caller was able to access the correct type.
close privID !_ = Fae $ do
  let PrivateEscrowID escrowID = privID
  escrowM <- use $ _transientState . _escrows . _useEscrows . at escrowID
  escrow <- maybe
    (throwIO $ BadEscrowID escrowID)
    return
    escrowM
  priv <- maybe
    (throwIO $ 
      BadPrivateType escrowID (typeRep $ Proxy @privT) (dynTypeRep $ private escrow))
    return $ 
    fromDynamic (private escrow)
  _transientState . _escrows . _useEscrows . at escrowID .= Nothing
  return priv


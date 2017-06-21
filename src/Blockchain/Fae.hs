module Blockchain.Fae 
  (
    Fae, EntryID, FacetID, EscrowID, Fee, Output, TransactionID,
    module Blockchain.Fae
  ) where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad.State

import Data.Dynamic
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable
import Data.Void

createPure :: (Typeable a, Typeable b) => (a -> Fae b) -> Fae EntryID
createPure f = create f const undefined

createMonoid :: (Monoid a, Typeable a, Typeable b) => (a -> Fae b) -> Fae EntryID
createMonoid f = create f mappend mempty

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
  lSeq <- use $ _transientState . _localLabel
  _transientState . _newOutput %= addOutput lSeq newEntryID
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
  when (inFacet entry /= curFacet) $
    throwIO $ WrongFacet entryID curFacet (inFacet entry)
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
      BadEntryValType entryID (typeRep $ Proxy @(Fae valT)) $
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

facet :: FacetID -> PrivateEscrowID Fee -> Fae ()
facet facetID feeID = Fae $ do
  curFacetID <- use $ _transientState . _currentFacet
  newFacetM <- use $ _persistentState . _facets . _useFacets . at facetID
  newFacet <- maybe
    (throwIO $ NotAFacet facetID)
    return
    newFacetM
  when (Set.notMember curFacetID $ depends newFacet) $
    throwIO $ NotADependentFacet curFacetID facetID
  feeGiven <- getFae $ close feeID FeeToken
  when (feeGiven < fee newFacet) $
    throwIO $ InsufficientFee facetID feeGiven (fee newFacet)
  _transientState . _currentFacet .= facetID

signer :: Fae PublicKey
signer = Fae $ use $ _transientState . _sender

label :: Text -> Fae a -> Fae a
label l s = Fae $ do
  _transientState . _localLabel %= flip snoc l
  sVal <- getFae s
  _transientState . _localLabel %= view _init
  return sVal

follow :: TransactionID -> Fae Output
follow txID = Fae $ do
  outputM <- use $ _persistentState . _outputs . _useOutputs . at txID
  maybe
    (throwIO $ BadTransactionID txID)
    return
    outputM


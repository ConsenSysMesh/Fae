module Blockchain.Fae 
  (
    Fae, EntryID, FacetID, EscrowID, Output, TransactionID,
    module Blockchain.Fae
  ) where

import Blockchain.Fae.Crypto
import Blockchain.Fae.Internal
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad.State

import Data.Dynamic
import Data.IORef.Lifted
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
create f c a = addEntry $ Entry (toDyn f) (toDyn c) (toDyn a) 

spend :: a -> Fae a
spend x = Fae $ do
  entryIDM <- use $ _transientState . _currentEntry
  maybe
    (throwIO NoCurrentEntry)
    (\entryID -> _transientState . _entryUpdates . _useEntries %= sans entryID)
    entryIDM
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
  curFacetRef <- use _currentFacet
  curFacet <- readIORef curFacetRef
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
  _transientState . _currentEntry ?= entryID
  faeVal <- fromDyn faeValD $
    throwIO $
      BadEntryValType entryID (typeRep $ Proxy @(Fae valT)) $
        head $ typeRepArgs $ last $ typeRepArgs $ dynTypeRep $ contract entry
  getFae faeVal 

escrow ::
  forall tokT pubT privT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  tokT -> (privT -> pubT) -> privT -> 
  Fae (EscrowID tokT pubT privT)
escrow tok pubF priv = do
  entryID <- insertEntry (_transientState . _escrows . _useEscrows) escrow
  return (PublicEscrowID entryID, PrivateEscrowID entryID)

  where
    escrow = Escrow (toDyn priv) (toDyn $ pubF priv) (toDyn tok) contractMaker
    contractMaker entryID key = toDyn f where
      f :: Signature -> Fae (Maybe (EscrowID tokT pubT privT))
      f sig
        | verifySig key entryID sig = Fae $ do
            _transientState . _escrows . _useEscrows . at entryID ?= escrow
            return $ Just (PublicEscrowID entryID, PrivateEscrowID entryID)
        | otherwise = return Nothing

peek ::
  forall tokT privT pubT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  PublicEscrowID tokT pubT privT -> Fae pubT
peek (PublicEscrowID escrowID) = 
  useEscrow
    (Proxy @tokT)
    (Proxy @pubT) _public BadPublicType
    (Proxy @privT) _private BadPrivateType
    escrowID

close ::
  forall tokT pubT privT.
  (Typeable tokT, Typeable pubT, Typeable privT) =>
  PrivateEscrowID tokT pubT privT -> tokT -> Fae privT
-- Need strictness here to force the caller to actually have a token, and not
-- just a typed 'undefined'.  The token itself is unused; we just need to
-- know that the caller was able to access the correct type.
close (PrivateEscrowID escrowID) !_ = do
  priv <- useEscrow
    (Proxy @tokT)
    (Proxy @privT) _private BadPrivateType
    (Proxy @pubT) _public BadPublicType
    escrowID
  Fae $ _transientState . _escrows . _useEscrows . at escrowID .= Nothing
  return priv

facet :: FacetID -> Fae ()
facet facetID = Fae $ do
  getFae saveTransient
  cFacetRef <- use _currentFacet
  curFacetID <- readIORef cFacetRef
  writeIORef cFacetRef facetID
  pStateRef <- use _persistentState
  pState <- readIORef pStateRef
  let newFacetM = pState ^. _facets . _useFacets . at facetID
  newFacet <- maybe
    (throwIO $ NotAFacet facetID)
    return
    newFacetM
  when (Set.notMember curFacetID $ depends newFacet) $
    throwIO $ NotADependentFacet curFacetID facetID

signer :: Fae PublicKey
signer = Fae $ use $ _transientState . _sender

label :: Text -> Fae a -> Fae a
label l s = Fae $ do
  oldLabel <- use $ _transientState . _localLabel
  let
    addLabel = _transientState . _localLabel %= flip snoc l
    popLabel = _transientState . _localLabel .= oldLabel
  bracket_ addLabel popLabel $ getFae s

follow :: TransactionID -> Fae Output
follow txID = Fae $ do
  pStateRef <- use _persistentState
  pState <- readIORef pStateRef
  let outputEM = pState ^. _outputs . _useOutputs . at txID
  maybe
    (throwIO $ BadTransactionID txID)
    (return . txOutput)
    outputEM


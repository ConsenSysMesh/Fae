module Blockchain.Fae 
  (
    Fae, EntryID, FacetID, EscrowID, Fee, Output, TransactionID,
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
  getFae $ output newEntryID
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
  forall tokT pubT privT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  tokT -> (privT -> pubT) -> privT -> 
  Fae (EscrowID tokT pubT privT)
escrow tok pubF priv = Fae $ do
  oldHash <- use $ _transientState . _lastHashUpdate
  let
    contractMaker :: EntryID -> PublicKey -> Signature -> Fae (Maybe (EscrowID tokT pubT privT))
    contractMaker entryID key sig
      | verifySig key entryID sig = Fae $ do
          _transientState . _escrows . _useEscrows . at entryID ?= escrow
          return $ Just (PublicEscrowID entryID, PrivateEscrowID entryID)
      | otherwise = return Nothing

    escrow = Escrow (toDyn priv) (toDyn $ pubF priv) (toDyn tok) (toDyn contractMaker)
    newHash = digestWith oldHash escrow
    entryID = EntryID newHash
  _transientState . _lastHashUpdate .= newHash
  _transientState . _escrows . _useEscrows . at entryID ?= escrow
  return (PublicEscrowID entryID, PrivateEscrowID entryID)

peek ::
  forall tokT privT pubT.
  (Typeable tokT, Typeable privT, Typeable pubT) =>
  PublicEscrowID tokT pubT privT -> Fae pubT
peek (PublicEscrowID escrowID) = Fae $ do
  escrowM <- use $ _transientState . _escrows . _useEscrows . at escrowID
  escrow <- maybe
    (throwIO $ BadEscrowID escrowID)
    return
    escrowM
  let
    tokTRep = typeRep (Proxy @tokT)
    pubTRep = typeRep (Proxy @pubT)
    privTRep = typeRep (Proxy @privT)
    realTokTRep = dynTypeRep (token escrow)
    realPubTRep = dynTypeRep (public escrow)
    realPrivTRep = dynTypeRep (private escrow)
  -- We check this to force the supplier to provide a type-correct escrow
  -- ID even though the private value is not returned.
  when (privTRep /= realPrivTRep) $ throwIO $
    BadPrivateType escrowID privTRep realPrivTRep
  -- Likewise here
  when (tokTRep /= realTokTRep) $ throwIO $
    BadTokenType escrowID tokTRep realTokTRep
  maybe
    (throwIO $ BadPublicType escrowID pubTRep realPubTRep)
    return 
    (fromDynamic $ public escrow)

close ::
  forall tokT pubT privT.
  (Typeable tokT, Typeable pubT, Typeable privT) =>
  PrivateEscrowID tokT pubT privT -> tokT -> Fae privT
-- Need strictness here to force the caller to actually have a token, and not
-- just a typed 'undefined'.  The token itself is unused; we just need to
-- know that the caller was able to access the correct type.
close (PrivateEscrowID escrowID) !_ = Fae $ do
  escrowM <- use $ _transientState . _escrows . _useEscrows . at escrowID
  escrow <- maybe
    (throwIO $ BadEscrowID escrowID)
    return
    escrowM
  let
    tokTRep = typeRep (Proxy @tokT)
    pubTRep = typeRep (Proxy @pubT)
    privTRep = typeRep (Proxy @privT)
    realTokTRep = dynTypeRep (token escrow)
    realPubTRep = dynTypeRep (public escrow)
    realPrivTRep = dynTypeRep (private escrow)
  -- We check this to force the supplier to provide a type-correct escrow
  -- ID even though the public value is not returned.
  when (pubTRep /= realPubTRep) $ throwIO $
    BadPublicType escrowID pubTRep realPubTRep
  -- This is a necessary check.  The strictness annotation ensures that we
  -- have a defined token argument, but it doesn't prove that the inferred
  -- type is actually the one in the escrow.
  when (tokTRep /= realTokTRep) $ throwIO $
    BadTokenType escrowID tokTRep realTokTRep
  priv <- maybe
    (throwIO $ BadPrivateType escrowID privTRep realPrivTRep)
    return 
    (fromDynamic $ private escrow)
  _transientState . _escrows . _useEscrows . at escrowID .= Nothing
  return priv

facet :: FacetID -> FeeEscrowID -> Fae ()
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
  _transientState . _currentFee .= feeGiven
  _transientState . _currentFeeLeft .= feeGiven
  when (feeGiven < fee newFacet) $
    throwIO $ InsufficientFee facetID feeGiven (fee newFacet)
  _transientState . _currentFacet .= facetID

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
  outputM <- use $ _persistentState . _outputs . _useOutputs . at txID
  maybe
    (throwIO $ BadTransactionID txID)
    return
    outputM


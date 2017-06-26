module Blockchain.Fae.Internal where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Control.Monad
import Control.Monad.State

import Data.Dynamic
import Data.Proxy
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.Map as Map

execFae :: Fae () -> FaeState -> IO FaeState
execFae = execStateT . getFae

output :: EntryID -> Fae ()
output entryID = Fae $ do
  lSeq <- use $ _transientState . _localLabel
  _transientState . _newOutput %= addOutput lSeq entryID

addOutput :: Seq Text -> EntryID -> Output -> Output
addOutput lSeq entryID output =
  maybe
    (output & _outputEntryID ?~ entryID)
    (\(label, rest) -> 
      output & 
        _subOutputs . 
        at label . 
        defaultLens (Output Nothing Map.empty) %~ 
        addOutput rest entryID
    )
    (uncons lSeq)

zeroFacet :: FacetID
zeroFacet = FacetID -- TBD

nullEntry :: EntryID
nullEntry = EntryID undefined -- TBD

addEntry :: (FacetID -> Entry) -> Fae EntryID
addEntry mkEntry = do
  facet <- Fae $ use $ _transientState . _currentFacet
  entryID <- 
    insertEntry (_transientState . _entryUpdates . _useEntries) $ mkEntry facet
  output entryID
  return entryID

insertEntry :: 
  (At m, Digestible (IxValue m), Index m ~ EntryID) => 
  Lens' FaeState m -> IxValue m -> Fae EntryID
insertEntry l x = do
  entryID <- newEntryID x
  Fae $ l . at entryID ?= x
  return entryID

newEntryID :: (Digestible a) => a -> Fae EntryID
newEntryID x = Fae $ do
  oldHash <- use $ _transientState . _lastHashUpdate
  let 
    newHash = digestWith oldHash x
    newEntryID = EntryID newHash
  _transientState . _lastHashUpdate .= newHash
  return newEntryID

useEscrow :: 
  forall tokT a b.
  (Typeable tokT, Typeable a, Typeable b) =>
  Proxy tokT ->
  Proxy a ->
  Lens' Escrow Dynamic -> -- Lens' Escrow a
  (EntryID -> TypeRep -> TypeRep -> EscrowException) ->
  Proxy b ->
  Lens' Escrow Dynamic -> -- Lens' Escrow b
  (EntryID -> TypeRep -> TypeRep -> EscrowException) ->
  EntryID -> Fae a
useEscrow 
  tokTP 
  retValTP retValLens retValErr 
  otherTP otherLens otherErr 
  escrowID 
  = Fae $ do

  escrowM <- use $ _transientState . _escrows . _useEscrows . at escrowID
  escrow <- maybe
    (throwIO $ BadEscrowID escrowID)
    return
    escrowM
  let
    realTokTRep = dynTypeRep (token escrow)
    realOtherTRep = dynTypeRep (escrow ^. otherLens)
    realRetValTRep = dynTypeRep (escrow ^. retValLens)

  -- We check this to force the supplier to provide a type-correct escrow
  -- ID even though the "other" value is not returned.
  when (otherTRep /= realOtherTRep) $ throwIO $
    otherErr escrowID otherTRep realOtherTRep
  -- Likewise here
  when (tokTRep /= realTokTRep) $ throwIO $
    BadTokenType escrowID tokTRep realTokTRep
  maybe
    (throwIO $ retValErr escrowID retValTRep realRetValTRep)
    return 
    (fromDynamic $ escrow ^. retValLens)

  where
    tokTRep = typeRep (Proxy @tokT)
    otherTRep = typeRep (Proxy @b)
    retValTRep = typeRep (Proxy @a)


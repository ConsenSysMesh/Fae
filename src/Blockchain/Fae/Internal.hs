module Blockchain.Fae.Internal where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Types

import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.Map as Map

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
addEntry mkEntry = Fae $ do
  facet <- use $ _transientState . _currentFacet
  oldHash <- use $ _transientState . _lastHashUpdate
  let 
    entry = mkEntry facet
    newHash = digestWith oldHash entry
    newEntryID = EntryID newHash
  getFae $ output newEntryID
  _transientState . _lastHashUpdate .= newHash
  _transientState . _entryUpdates . _useEntries . at newEntryID ?= entry 
  return newEntryID


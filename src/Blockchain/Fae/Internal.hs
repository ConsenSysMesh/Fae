module Blockchain.Fae.Internal where

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

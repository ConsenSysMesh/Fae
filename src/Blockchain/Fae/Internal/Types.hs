module Blockchain.Fae.Internal.Types where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Applicative
import Control.Exception

import Control.Monad
import Control.Monad.State

import Data.Dynamic
import Data.Functor
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.Map as Map

import Numeric.Natural

newtype Fae a = 
  Fae { getFae :: StateT FaeState IO a }
  deriving (Functor, Applicative, Monad)

data FaeState =
  FaeState
  {
    persistentState :: FaePersist,
    transientState :: FaeTransient,
    parameters :: FaeParameters
  }

data FaePersist =
  FaePersist 
  {
    entries :: Entries,
    facets :: Facets,
    outputs :: Outputs,
    lastHash :: Digest
  }

data FaeTransient =
  FaeTransient
  {
    entryUpdates :: Entries,
    newOutput :: Output,
    escrows :: Escrows,    
    sender :: PublicKey,
    lastHashUpdate :: Digest,
    currentEntry :: EntryID,
    currentFacet :: FacetID,
    currentFee :: Fee,
    currentFeeLeft :: Fee,
    localLabel :: Seq Text
  }

data FaeParameters =
  FaeParameters

newtype Entries = 
  Entries
  {
    useEntries :: Map EntryID Entry
  }

newtype Facets =
  Facets
  {
    useFacets :: Map FacetID Facet
  }

newtype Outputs =
  Outputs
  {
    useOutputs :: Map TransactionID Output
  }
data TransactionID = TransactionID -- TBD
  deriving (Eq, Ord, Show)

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EscrowID Escrow
  }

data Entry =
  Entry
  {
    contract :: Dynamic, -- :: accumT -> Fae valT
    combine :: Dynamic, -- :: argT -> accumT -> accumT
    accum :: Dynamic, -- :: accumT
    inFacet :: FacetID
  }
newtype EntryID = EntryID Digest deriving (Eq, Ord, Show)

data Output =
  Output
  {
    outputEntryID :: Maybe EntryID,
    subOutputs :: Map Text Output
  }

data Facet =
  Facet
  {
    fee :: Fee,
    depends :: Set FacetID -- all transitive dependencies
  }
data FacetID = FacetID -- TBD
  deriving (Eq, Ord, Show)

data Escrow =
  Escrow
  {
    private :: Dynamic, -- privT
    public :: Dynamic, -- pubT
    token :: Dynamic -- tokT
  }
newtype EscrowID = EscrowID Digest deriving (Eq, Ord, Show)
newtype PublicEscrowID privT = PublicEscrowID EscrowID
newtype PrivateEscrowID privT = PrivateEscrowID EscrowID

newtype Fee = Fee Natural deriving (Eq, Ord, Show)
data FeeToken = FeeToken

-- TH 
makeLenses ''Entries
makeLenses ''FaeParameters
makeLenses ''FaeTransient
makeLenses ''FaePersist
makeLenses ''FaeState
makeLenses ''Facets
makeLenses ''Outputs
makeLenses ''Escrows
makeLenses ''Entry
makeLenses ''Facet
makeLenses ''Output
makeLenses ''Escrow

-- Instances

instance Digestible EntryID where
instance Digestible Entry where

instance Digestible EscrowID where
instance Digestible Escrow where

-- Simple functions

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

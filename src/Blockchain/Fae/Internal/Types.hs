module Blockchain.Fae.Internal.Types where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Applicative
import Control.Exception.Safe

import Control.Monad
import Control.Monad.Fix
import Control.Monad.State

import Data.Dynamic
import Data.Functor
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

import qualified Data.Map as Map

import GHC.Generics

import Numeric.Natural

newtype Fae a = 
  Fae { getFae :: StateT FaeState IO a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadThrow, MonadCatch)

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
    currentEntry :: Maybe EntryID,
    currentFacet :: FacetID,
    currentFee :: Fee,
    currentFeeLeft :: Fee,
    localLabel :: Seq Text
  }

data FaeParameters =
  FaeParameters
  {
    transactionCredit :: Fee
  }

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
    useOutputs :: Map TransactionID (Either SomeException Output)
  }
data TransactionID = TransactionID -- TBD
  deriving (Eq, Ord, Show, Generic)
instance Serialize TransactionID
instance Digestible TransactionID

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EntryID Escrow
  }

data Entry =
  Entry
  {
    contract :: Dynamic, -- :: accumT -> Fae valT
    combine :: Dynamic, -- :: argT -> accumT -> accumT
    accum :: Dynamic, -- :: accumT
    inFacet :: FacetID
  }
newtype EntryID = EntryID Digest deriving (Eq, Ord, Show, Generic)
instance Serialize EntryID
instance Digestible EntryID

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
newtype FacetID = FacetID Natural deriving (Eq, Ord, Show)

data Escrow =
  Escrow
  {
    private :: Dynamic, -- privT
    public :: Dynamic, -- pubT
    token :: Dynamic, -- tokT
    contractMaker :: EntryID -> PublicKey -> Dynamic -- Signature -> EscrowID tokT pubT privT
  }
newtype PublicEscrowID tokT pubT privT = PublicEscrowID EntryID
newtype PrivateEscrowID tokT pubT privT = PrivateEscrowID EntryID
type EscrowID tokT pubT privT =
  (PublicEscrowID tokT pubT privT, PrivateEscrowID tokT pubT privT)

newtype Fee = Fee { getFee :: Natural } deriving (Eq, Ord, Show)
data FeeToken = FeeToken
type FeeEscrowID = PrivateEscrowID FeeToken Natural Fee

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


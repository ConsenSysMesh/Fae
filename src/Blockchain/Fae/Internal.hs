module Blockchain.Fae.Internal where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Applicative

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Dynamic
import Data.Functor

import Data.Map (Map)
import qualified Data.Map as Map

import Numeric.Natural

-- Types

newtype Fae a = 
  Fae (StateT FaeState (ExceptT FaeException IO) a) 
  deriving (Functor, Applicative, Monad)

data FaeException =
  HaskellException String |
  BadContractType |
  BadEscrowType |
  InvalidNewFacet |
  InvalidFeeType |
  InsufficientFee |
  ResourcesExhausted
  deriving (Show)

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
    contracts :: Entries,
    facets :: Facets
  }

data FaeTransient =
  FaeTransient
  {
    escrows :: Escrows,    
    sender :: PublicKey,
    currentEntry :: Entry,
    currentFacet :: Facet,
    currentFee :: Fee,
    currentFeeLeft :: Fee
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

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EscrowID Escrow
  }

data Entry =
  Entry
  {
    entryID :: EntryID,
    contract :: Dynamic, -- a Contract argT accumT valT
    facet :: FacetID
  }
data EntryID = EntryID -- some type of hash, TBD

data Facet =
  Facet
  {
    facetID :: FacetID,
    fee :: Fee,
    depends :: [FacetID]
  }
data FacetID = FacetID -- TBD

newtype Escrow =
  Escrow
  {
    account :: Dynamic -- an EscrowAccount tokT privT pubT
  }
data EscrowID = EscrowID -- TBD

data Contract argT accumT valT =
  Contract
  {
    contractF :: accumT -> valT,
    combine :: argT -> accumT -> accumT,
    accum :: accumT
  }

data EscrowAccount tokT privT pubT =
  EscrowAccount
  {
    private :: privT,
    public :: pubT    
  }

newtype Fee = Fee Natural

-- TH 
makeLenses ''Entries
makeLenses ''FaeParameters
makeLenses ''FaeTransient
makeLenses ''FaePersist
makeLenses ''FaeState
makeLenses ''Facets
makeLenses ''Escrows
makeLenses ''Entry
makeLenses ''Facet
makeLenses ''Escrow
makeLenses ''Contract
makeLenses ''EscrowAccount

-- Instances

instance Digestible EntryID where

-- Values

create :: (accumT -> valT) -> (argT -> accumT -> accumT) -> accumT -> Fae EntryID
create = undefined

module Blockchain.Fae.Internal.Types where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Applicative

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.State

import Data.Dynamic
import Data.Functor
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Text (Text)

{- The main monad -}

newtype Fae a = 
  Fae { useFae :: StateT FaeState IO a }
  -- Don't derive 'MonadState'!
  deriving (Functor, Applicative, Monad, MonadFix, MonadThrow, MonadCatch)

data FaeState =
  FaeState
  {
    transactions :: Transactions,
    contracts :: Contracts,
    escrows :: Escrows,
    transientState :: FaeTransient,
    parameters :: FaeParameters
  }

data FaeTransient =
  FaeTransient
  {
    contractUpdates :: Contracts,
    escrowUpdates :: Escrows,
    sender :: PublicKey,
    lastHash :: Digest
  }

data FaeParameters =
  FaeParameters
  {
  }

{- Transactions -}

newtype Transactions = 
  Transactions
  {
    useTransactions :: Map TransactionID (Either SomeException Dynamic)
  }

newtype TransactionID = TransactionID Digest deriving (Eq, Ord, Show)

{- Contracts -}

newtype Contracts = 
  Contracts
  {
    useContracts :: Map ContractID (Either SomeException AbstractContract)
  }

type AbstractContract = Dynamic -> Fae Dynamic

data Contract argType accumType valType =
  Contract
  {
    inputs :: Fae (Seq Dynamic), -- Invocation return values
    result :: DataF argType accumType
      (
        [ContractID -> AbstractContract], -- Created contracts
        valType,
        accumType
      ),
    accum :: accumType
  }

type DataF argType accumType a = Seq Dynamic -> argType -> accumType -> Fae a
newtype ContractID = ContractID Digest deriving (Eq, Ord, Show)

{- Escrows -}

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EntryID Dynamic -- Escrow tok pub priv
  }

data Escrow tokType pubType privType =
  Escrow
  {
    private :: privType,
    public :: pubType
  }

newtype EntryID = EntryID Digest deriving (Eq, Ord, Show)
newtype PublicEscrowID tokT pubT privT = PublicEscrowID EntryID
newtype PrivateEscrowID tokT pubT privT = PrivateEscrowID EntryID
type EscrowID tokT pubT privT =
  (PublicEscrowID tokT pubT privT, PrivateEscrowID tokT pubT privT)

-- TH 
makeLenses ''FaeParameters
makeLenses ''FaeTransient
makeLenses ''FaeState
makeLenses ''Transactions
makeLenses ''Contracts
makeLenses ''Contract
makeLenses ''Escrows
makeLenses ''Escrow

uniqueDigest :: (Digestible a) => a -> Fae Digest
uniqueDigest x = Fae $ do
  h <- use $ _transientState . _lastHash
  let newHash = h <> digest x
  _transientState . _lastHash .= newHash
  return newHash

signer :: Fae PublicKey
signer = Fae $ use $ _transientState . _sender


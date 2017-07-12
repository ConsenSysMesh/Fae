module Blockchain.Fae.Internal.Fae where

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
    useTransactions :: Map TransactionID (Either SomeException Dynamic) -- valType
  }

newtype TransactionID = TransactionID Digest deriving (Eq, Ord, Show)

{- Contracts -}

newtype Contracts = 
  Contracts
  {
    useContracts :: Map ContractID (Either SomeException AbstractContract)
  }

type AbstractContract = Dynamic -> Fae Dynamic -- argType -> Fae valType
newtype ContractID = ContractID Digest deriving (Eq, Ord, Show)

{- Escrows -}

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EntryID Dynamic -- Escrow tok pub priv
  }

newtype EntryID = EntryID Digest deriving (Eq, Ord, Show)

-- TH 
makeLenses ''FaeParameters
makeLenses ''FaeTransient
makeLenses ''FaeState
makeLenses ''Transactions
makeLenses ''Contracts
makeLenses ''Escrows

uniqueDigest :: (Digestible a) => a -> Fae Digest
uniqueDigest x = Fae $ do
  h <- use $ _transientState . _lastHash
  let newHash = h <> digest x
  _transientState . _lastHash .= newHash
  return newHash

signer :: Fae PublicKey
signer = Fae $ use $ _transientState . _sender


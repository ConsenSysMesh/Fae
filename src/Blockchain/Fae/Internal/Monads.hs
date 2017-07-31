module Blockchain.Fae.Internal.Monads where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State

import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq)

import GHC.Generics

{- Internal monads -}

data ContractID =
  JustTransaction TransactionID |
  TransactionOutput TransactionID Int |
  InputOutput TransactionID ShortContractID Int
  deriving (Show, Generic)

instance Serialize ContractID
instance Digestible ContractID

newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Show, Serialize)

instance Digestible ShortContractID

type TransactionID = ShortContractID -- For simplicity

newtype Storage = Storage { getStorage :: Map TransactionID TransactionEntry }

data TransactionEntry =
  TransactionEntry 
  {
    inputContracts :: Map ShortContractID Outputs,
    outputs :: Outputs,
    returnValue :: Dynamic
  }

type Outputs = IntMap AbstractContract

type FaeBlock a = StateT Storage IO a

data ContractData =
  ContractData
  {
    inputs :: Seq (ContractID, Dynamic),
    contractID :: ContractID,
    outputID :: Int -> ContractID,
    transactionKey :: PublicKey
  }

data OutputData a =
  OutputData 
  {
    updatedContract :: Maybe AbstractContract,
    outputContracts :: Seq AbstractContract,
    retVal :: a,
    newEscrow :: Maybe (EntryID, Dynamic)
  }

newtype FaeContract a = FaeContract { getFaeContract :: Reader ContractData a }
  deriving (Functor, Applicative, Monad)

data InputData a =
  InputData
  {
    inputArg :: a,
    inputEscrow :: Maybe (EntryID, Dynamic)
  }

type AbstractContract = InputData Dynamic -> FaeContract (OutputData Dynamic)

{- Monad for contract authors -}

type ConcreteContract argType valType = 
  InputData argType -> FaeContract (OutputData valType)

newtype EntryID = EntryID Digest
  deriving (Eq, Ord, Show)

newtype EscrowID argType valType = EscrowID EntryID deriving (Typeable)
newtype Escrow argType valType = Escrow (ConcreteContract argType valType)

data AnyEscrowID = 
  forall argType valType. 
  (Typeable argType, Typeable valType) =>
  SomeEscrowID (EscrowID argType valType)

newtype PrivateEscrowID argType valType = 
  PrivateEscrowID EntryID deriving (Typeable)
newtype PrivateEscrow argType valType = 
  PrivateEscrow (ConcreteContract argType valType)

type Escrows = Map EntryID Dynamic -- Escrow argType valType

data StateData accumType =
  StateData
  {
    escrows :: Escrows,
    accum :: accumType,
    spent :: Bool
  }

newtype Fae argType accumType valType =
  Fae 
  {
    getFae :: RWST argType (Seq AbstractContract) (StateData accumType) 
             FaeContract valType
  }
  deriving (Functor, Applicative, Monad, MonadReader argType)

type AnyFae valType = 
  forall argType accumType. 
  (Typeable argType, Typeable valType) =>
  Fae argType accumType valType

-- TH
makeLenses ''Storage
makeLenses ''TransactionEntry
makeLenses ''ContractData
makeLenses ''OutputData
makeLenses ''StateData

shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

instance MonadState accumType (Fae argType accumType) where
  get = Fae $ use _accum
  put = Fae . (_accum .=)

sender :: Fae argType accumType PublicKey
sender = Fae $ lift $ FaeContract $ view _transactionKey


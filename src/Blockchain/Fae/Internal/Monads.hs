{-# LANGUAGE UndecidableInstances #-}
module Blockchain.Fae.Internal.Monads where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Monad
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Class

import Data.Dynamic
import Data.Foldable
import Data.Functor.Identity
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq)

import GHC.Generics

{- Blockchain storage -}

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

newtype Storage =
  Storage 
  { 
    getStorage :: Map TransactionID TransactionEntry
  }

data TransactionEntry =
  TransactionEntry 
  {
    inputOutputs :: InputOutputs,
    outputs :: Outputs,
    result :: Dynamic
  }

type InputOutputs = Map ShortContractID Outputs
type Outputs = IntMap TrustContract

data TrustContract = 
  TrustContract
  {
    abstractContract :: AbstractContract,
    trusted :: [ShortContractID]
  }

type FaeBlock a = StateT Storage IO a

{- Escrows -}

type EntryID = (TransactionID, Int)
type Escrows = Map EntryID AbstractContract

data EscrowID argType valType = EscrowID EntryID

data AnEscrowID = forall argType valType. AnEscrowID (EscrowID argType valType)

anEscrowID :: EscrowID argType valType -> AnEscrowID
anEscrowID eID = AnEscrowID eID

class HasEscrowIDs a where
  getEscrowIDs :: a -> [AnEscrowID]

instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  getEscrowIDs _ = []

instance HasEscrowIDs (EscrowID argType valType) where
  getEscrowIDs = (:[]) . anEscrowID

instance (HasEscrowIDs a, HasEscrowIDs b) => HasEscrowIDs (a, b) where
  getEscrowIDs (x, y) = getEscrowIDs x ++ getEscrowIDs y

instance {-# OVERLAPPABLE #-} 
  (HasEscrowIDs a, Foldable t) => HasEscrowIDs (t a) where

  getEscrowIDs = concatMap getEscrowIDs . toList

data WithEscrows a = WithEscrows Escrows a
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

{- Contract execution -}

newtype Wrapped m a = Wrapped { unWrapped :: m a }
  deriving (Functor, Applicative, Monad)

type FaeContractRWST s = Coroutine s (RWS PublicKey [TrustContract] EntryID)
type FaeContractStateT s = 
  Coroutine s (StateT Escrows (Wrapped (RWS PublicKey [TrustContract] EntryID)))

instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
  state = lift . state

instance (Functor f, MonadReader r m) => MonadReader r (Coroutine f m) where
  ask = lift ask
  local = liftT . local

instance (Functor f, MonadWriter w m) => MonadWriter w (Coroutine f m) where
  tell = lift . tell
  listen = liftT listen
  pass = liftT pass

deriving instance (Monoid w) => MonadWriter w (Wrapped (RWS r w s))

liftT :: (Monad m, MonadTrans t, Monad (t m)) => (m a -> m b) -> (t m a -> t m b)
liftT f x = x >>= lift . f . return

newtype ConcreteContract argType valType = 
  ConcreteContract
  (
    forall s. (Functor s) => 
      argType -> FaeContractStateT s (Maybe AbstractContract, valType)
  )

type AbstractContract = ConcreteContract Dynamic Dynamic

-- TH
makeLenses ''Storage
makeLenses ''TransactionEntry
makeLenses ''TrustContract

shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest


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
type Outputs = Seq TrustContract

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

newtype EscrowID argType valType = EscrowID EntryID
newtype AnEscrowID = AnEscrowID EntryID

class HasEscrowIDs a where
  getEscrowIDs :: a -> [AnEscrowID]

instance {-# OVERLAPPABLE #-} HasEscrowIDs a where
  getEscrowIDs _ = []

data WithEscrows a = WithEscrows Escrows a
type FaeRequest argType valType = 
  Request (WithEscrows valType) (WithEscrows argType)

{- Contract execution -}

newtype Wrapped m a = Wrapped { unWrapped :: m a }
  deriving (Functor, Applicative, Monad)

type FaeContractRWST s = Coroutine s (RWS PublicKey Outputs EntryID)
type FaeContractStateT s = 
  Coroutine s (StateT Escrows (Wrapped (RWS PublicKey Outputs EntryID)))
type FaeM argType valType = FaeContractStateT (FaeRequest argType valType)

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

{- Contract authoring -}

type Fae argType valType = Wrapped (FaeM argType valType)
type AnyFae a = forall s. (Functor s) => Wrapped (FaeContractStateT s) a

type Contract argType valType = 
  argType -> Fae argType valType (WithEscrows valType)

newtype Inputs = Inputs (Seq Dynamic)
type Transaction a = Inputs -> Wrapped (FaeContractStateT Naught) a

-- TH
makeLenses ''Storage
makeLenses ''TransactionEntry
makeLenses ''TrustContract

shorten :: ContractID -> ShortContractID
shorten = ShortContractID . digest

sender :: Fae argType valType PublicKey
sender = Wrapped $ lift $ lift $ Wrapped ask


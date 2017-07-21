module Blockchain.Fae.Internal.Monads where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Lens

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State

import Data.Dynamic
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq 
import Data.Sequence (Seq)

{- Internal monads -}

newtype CallPath = CallPath (Seq ShortContractID)
  deriving (Show, Serialize)

instance Digestible CallPath

newtype ShortContractID = ShortContractID Digest
  deriving (Eq, Ord, Show, Serialize)

data ContractID =
  ContractID
  {
    path :: CallPath,
    basename :: Integer
  }
  deriving (Show)

data Contracts = 
  Contracts
  {
    inputStorage :: Map ShortContractID Contracts,
    outputStorage :: Map Integer AbstractContract
  }

type AbstractContract = Dynamic -> FaeContract Dynamic

newtype FaeBlock a = FaeBlock (StateT Contracts IO a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch)

newtype FaeTX a = FaeTX (RWS PublicKey () Contracts a)
  deriving (Functor, Applicative, Monad)

data ContractData =
  ContractData
  {
    callPath :: CallPath,
    contractID :: ContractID,
    escrowArg :: Maybe (EntryID, Dynamic)
  }

newtype FaeContract a = FaeContract (ReaderT ContractData FaeTX a)
  deriving (Functor, Applicative, Monad)

{- Monad for contract authors -}

newtype EntryID = EntryID Int
  deriving (Eq, Ord, Show)

newtype Escrows =
  Escrows
  {
    useEscrows :: Map EntryID Dynamic -- Escrow tok pub priv
  }

data InputData argType = 
  InputData
  {
    inputValues :: Seq (ContractID, Dynamic),
    arg :: argType
  }

data StateData accumType =
  StateData
  {
    escrows :: Escrows,
    accum :: accumType
  }

newtype Fae argType accumType valType =
  Fae (RWS (InputData argType) [AbstractContract] (StateData accumType) valType)
  deriving (Functor, Applicative, Monad)

-- TH
makeLenses ''Contracts
makeLenses ''Escrows
makeLenses ''ContractData
makeLenses ''InputData
makeLenses ''StateData

shorten :: ContractID -> ShortContractID
shorten ContractID{..} = ShortContractID $ digest path <#> basename

type instance Index Contracts = ContractID
type instance IxValue Contracts = AbstractContract
instance Ixed Contracts 
instance At Contracts where
  at (ContractID (CallPath path) basename) =
    case uncons path of
      Nothing -> 
        _outputStorage . at basename
      Just (sID, rest) -> 
        _inputStorage . 
        at sID . 
        defaultLens (Contracts Map.empty Map.empty) .
        at (ContractID (CallPath rest) basename)

instance MonadReader argType (Fae argType accumType) where
  ask = Fae $ view _arg
  local f = pushFaeState (_arg %~ f)

instance MonadState accumType (Fae argType accumType) where
  get = Fae $ use _accum
  put = Fae . (_accum .=)

runTX :: 
  (Typeable a) =>
  PublicKey -> Contracts -> ContractData -> FaeContract a -> Contracts
runTX k s d (FaeContract m) = fst $ execRWS m' k s
  where
    FaeTX m' = do
      retVal <- runReaderT m d
      FaeTX $ at (contractID d) ?= const (return $ toDyn retVal)

pushContractState :: 
  (ContractData -> ContractData) -> FaeContract a -> FaeContract a
pushContractState f (FaeContract m) = FaeContract $ local f m

pushFaeState :: 
  (InputData argType -> InputData argType) -> 
  Fae argType accumType valType -> 
  Fae argType accumType valType
pushFaeState f (Fae m) = Fae $ local f m


{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Storage where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens

import Control.Monad.State

import Data.Dynamic
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Serialize

import qualified Data.IntMap as IntMap

{- Types -}

newtype StorageT c =
  Storage 
  { 
    getStorage :: Map TransactionID (TransactionEntryT c)
  }

data TransactionEntryT c =
  TransactionEntry 
  {
    inputOutputs :: InputOutputsT c,
    outputs :: OutputsT c,
    result :: Dynamic
  }

type InputOutputsT c = Map ShortContractID (OutputsT c)
type OutputsT c = IntMap c
type FaeStorageT c = StateT (StorageT c) IO

{- TH -}

makeLenses ''StorageT
makeLenses ''TransactionEntryT

{- Instances -}

type instance Index (StorageT c) = ContractID
type instance IxValue (StorageT c) = c
instance Ixed (StorageT c)
instance At (StorageT c) where
  at cID@(JustTransaction txID) = throw (BadContractID cID)

  at cID@(TransactionOutput txID i) =
    _getStorage .
    at txID .
    defaultLens (throw $ BadTransactionID txID) .
    _outputs .
    at i

  at cID@(InputOutput txID sID i) = 
    _getStorage .
    at txID .
    defaultLens (throw $ BadTransactionID txID) .
    _inputOutputs .
    at sID .
    defaultLens (throw $ BadInputID sID) .
    at i

{- Functions -}

intMapList :: [a] -> IntMap a
intMapList = IntMap.fromList . zip [0 ..]


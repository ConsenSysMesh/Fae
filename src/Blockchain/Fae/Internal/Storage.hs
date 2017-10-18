{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Storage where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens

import Control.Monad.IO.Class
import Control.Monad.State

import Data.Dynamic
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Data.Serialize

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

{- Types -}

data StorageT c =
  Storage 
  { 
    getStorage :: Map TransactionID (TransactionEntryT c),
    txLog :: [TransactionID] -- In reverse!
  }

data TransactionEntryT c =
  forall a. (Show a) =>
  TransactionEntry 
  {
    inputOutputs :: InputOutputsT c,
    outputs :: OutputsT c,
    result :: a
  }

type InputOutputsT c = Map ShortContractID (OutputsT c)
type OutputsT c = IntMap c
newtype FaeStorageT c a = FaeStorage {getFaeStorage :: StateT (StorageT c) IO a}
  deriving 
  (
    Functor, Applicative, Monad, 
    MonadThrow, MonadCatch, MonadMask,
    MonadIO
  )

deriving instance MonadState (StorageT c) (FaeStorageT c)

-- Exception type
data StorageException =
  BadTransactionID TransactionID |
  BadContractID ContractID |
  BadInputID ShortContractID
  deriving (Typeable, Show)

{- TH -}

makeLenses ''StorageT
makeLenses ''TransactionEntryT

{- Instances -}

instance Exception StorageException

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

showTransaction :: TransactionID -> FaeStorageT c String
showTransaction txID = do
  TransactionEntry ios os x <- use $
    _getStorage . at txID . defaultLens (throw $ BadTransactionID txID)
  return $ 
    intercalate "\n  " $
      ("Transaction " ++ show txID ++ ": " ++ showOutputs os) :
      ("result: " ++ show x) :
      (flip map (Map.toList ios) $ \(cID, os) ->
        "input " ++ show cID ++ ": " ++ showOutputs os)
  where showOutputs os = show (IntMap.size os) ++ " outputs"

showTransactions :: FaeStorageT c [String]
showTransactions = do
  txIDs <- gets $ reverse . txLog
  mapM showTransaction txIDs

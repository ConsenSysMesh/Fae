{- |
Module: Blockchain.Fae.Internal.Storage
Description: Storage of contracts and transactions
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides types and associated functions for accessing the storage of transactions and the contracts they create.
-}
{-# LANGUAGE TemplateHaskell #-}
module Blockchain.Fae.Internal.Storage where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs

import Common.Lens 

import Control.Applicative
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Trans

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe
import Data.Typeable
import Data.Vector (Vector)

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector (unsafeWrite)

import GHC.Generics (Generic)

-- * Types

-- | 'Storage' records the complete effects of running each transaction.
-- It also holds any contract return values that have been imported; no
-- attempt is made to validate them against the actual return values, and
-- it is expected that users come to some agreement on their correctness
-- before sharing them.
data Storage = 
  Storage 
  { 
    getStorage :: Map TransactionID TransactionEntry,
    importedValues :: Map ContractID InputResults
  }

-- | A general storage transformer; used for running transactions in IO.
type FaeStorageT = StateT Storage
-- | A newtype so that the interpreter doesn't need 'StateT' in scope.
newtype FaeStorage a = FaeStorage { getFaeStorage :: State Storage a }

-- | Each transaction can produce outputs in two different ways (cf.
-- 'ContractID'), has an associated "signer" public key, and a result.
--
-- The technical reason for separating 'inputOutputs' from 'outputs' is
-- that it makes it possible for contracts to create new contracts without
-- putting a global lock on the storage; thunk execution will go straight
-- to the correct contract's outputs and not require evaluation of any
-- other contract or transaction.
data TransactionEntry =
  TransactionEntry 
  {
    inputResults :: Vector InputResults,
    outputs :: Outputs,
    txSigners :: Signers,
    result :: Result,
    inputMaterials :: Materials
  }

-- | The result can be anything, but should be 'show'able so that it has
-- outside meaning.  This is an existential type, so the record names are
-- just there for documentation; values have to be extracted by
-- pattern-matching.
--
-- The constructor is strict so that there is no possibility of a hidden
-- exception in the value.
data Result = forall a. (Show a) => Result !a

-- | We save the versions map, with the actual values scrubbed, so that it
-- can be displayed to learn the actual version IDs.
data InputResults =
  InputResults
  {
    iRealID :: ContractID,
    iStatus :: Status,
    iVersionID :: VersionID,
    -- | This is /very dangerous/ because these escrows are duplicates.
    -- This value /must not/ be used inside Fae, but only exposed to
    -- external applications (such as import/export).
    iResult :: WithEscrows ReturnValue,
    iOutputsM :: Maybe Outputs,
    iMaterialsM :: Maybe Materials
  } deriving (Generic)

data Status = Updated | Deleted | Failed deriving (Show, Generic)

-- | The elements are 'Maybe' because an output contract may be deleted,
-- but the indexing of the others should not change, so we have to keep the
-- full array.
type Outputs = Vector Output

-- | Record of the "materials" (extra contract calls) given to
-- a transaction or contract, in order of declaration but also tagged by
-- name.
type Materials = Vector (String, InputResults)

-- | Not only convenient, but also important for ensuring that the three
-- different source trees using this type all have the same version of it.
-- Since this type is exchanged in serialized form between different
-- processes, type checking cannot verify it at compile time.
data ExportData = 
  ExportData
  {
    exportedCID :: ContractID,
    exportStatus :: Status,
    neededModules :: [String],
    exportNameType :: String,
    exportedValue :: ByteString
  }
  deriving (Generic)

-- * Template Haskell

makeLenses ''TransactionEntry
makeLenses ''InputResults
makeLenses ''Storage

{- Instances -}

-- | For convenience, so we don't have to pattern-match elsewhere.
instance Show Result where
  show (Result x) = show x

-- | -
instance Serialize Status

-- | -
instance NFData Status

-- | -
instance Serialize ExportData

-- * Functions

-- | A getter/dispatcher for the storage contents, using Church encoding
-- for 'Either' to avoid strict pattern-matches on the return value.
atCID :: 
  ContractID -> (InputResults -> a) -> (StoredContract -> a) -> a -> Storage -> a
atCID cID ivF scF a0 st
  | Just o <- st ^. outputAt cID = maybe a0 scF $ storedContract o
  | otherwise = maybe a0 ivF $ st ^. _importedValues . at cID

outputAt :: ContractID -> Lens' Storage (Maybe Output)
outputAt cID@ContractID{..} =
  _getStorage .
  at parentTransaction .
  uncertain (txPartLens transactionPart) .
  vectorAt creationIndex

txPartLens :: TransactionPart -> Lens' TransactionEntry (Maybe Outputs)
txPartLens p = 
  case p of
    Body -> _outputs . onlyJust DeletedEntry
    InputCall n -> 
      txInputLens n . 
      uncertain _iOutputsM

txInputLens :: Int -> Lens' TransactionEntry (Maybe InputResults)
txInputLens n = _inputResults . onlyJust DeletedEntry . vectorAt n

vectorAt :: Int -> Lens' (Maybe (Vector a)) (Maybe a)
vectorAt n = 
  lens ((=<<) (Vector.!? n)) (\mv my -> mv >>= (my >>=) . setter)
  where
    setter v y
      | 0 <= n && n < Vector.length v = 
        Just $ Vector.modify (\w -> Vector.unsafeWrite w n y) v
      | otherwise = Nothing

uncertain :: (Monad m) => Lens' s (m a) -> Lens' (m s) (m a)
uncertain l = lens getter setter where
  getter = (>>= view l)
  setter ms my = (l .~ my) <$> ms

onlyJust :: (Exception e) => e -> Lens' a (Maybe a)
onlyJust err = lens getter setter where
  getter = Just
  setter _ = fromMaybe $ throw err

-- | Deserializes an exported value as the correct type and puts it in
-- imported value storage for the future.  This is in `FaeStorage` and not
-- `FaeStorageT` because the former is not an instance of the latter, and
-- the interpreter benefits from having a monomorphic type.
addImportedValue :: 
  (HasEscrowIDs a, Exportable a) => 
  State Escrows a -> ContractID -> Status -> FaeStorage ()
addImportedValue valueImporter iRealID iStatus = FaeStorage $ 
  case contractVersion iRealID of
    Current -> throw $ ImportWithoutVersion iRealID
    Version iVersionID -> 
      _importedValues . at iRealID ?= 
        InputResults{iOutputsM = Nothing, iMaterialsM = Nothing, ..}
  where 
    iResult = WithEscrows escrowMap (ReturnValue importedValue)
    (importedValue, Escrows{..}) = 
      runState valueImporter (Escrows Map.empty nullDigest)

getExportedValue :: (Monad m) => TransactionID -> Int -> FaeStorageT m ExportData
getExportedValue txID ix = do
  InputResults{..} <- use $ getInputResults .
    defaultLens (throw $ BadInputID txID ix)
  Output{..} <- use $ outputAt iRealID . 
    defaultLens (throw $ BadContractID iRealID)
  let neededModules = tyConModule <$> listTyCons contractNameType
      WithEscrows eMap result = iResult
      exportedValue = evalState (exportReturnValue result) eMap
  return ExportData 
    {
      exportedCID = iRealID,
      exportStatus = iStatus,
      exportNameType = show contractNameType,
      ..
    }

  where
    getInputResults :: Lens' Storage (Maybe InputResults)
    getInputResults = 
      _getStorage . 
      at txID .
      uncertain (txInputLens ix)

    listTyCons :: TypeRep -> [TyCon]
    listTyCons rep = con : (reps >>= listTyCons) where
      (con, reps) = splitTyConApp rep

runStorageT :: (Monad m) => FaeStorageT m a -> m a
runStorageT = flip evalStateT $ Storage Map.empty Map.empty


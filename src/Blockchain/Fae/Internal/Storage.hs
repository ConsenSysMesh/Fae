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
import Blockchain.Fae.Internal.Versions

import Common.Lens 

import Control.Applicative
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
    importedValues :: Map ContractID (WithEscrows ReturnValue, VersionMap, Bool)
  }

-- | A general storage transformer; used for running transactions in IO.
newtype FaeStorageT m a = FaeStorageT { getFaeStorageT :: StateT Storage m a }
  deriving 
    (
      Functor, Applicative, Monad, 
      MonadThrow, MonadCatch, MonadMask, MonadIO
    )
-- | The version used for running pure transactions
newtype FaeStorage a = FaeStorage { getFaeStorage :: State Storage a }
  deriving (Functor, Applicative, Monad)

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
    result :: Result
  }

-- | The result can be anything, but should be 'show'able so that it has
-- outside meaning.  This is an existential type, so the record names are
-- just there for documentation; values have to be extracted by
-- pattern-matching.
data Result = forall a. (Show a) => Result a

-- | We save the versions map, with the actual values scrubbed, so that it
-- can be displayed to learn the actual version IDs.
data InputResults =
  InputResults
  {
    -- | The 'Left' means the contract was deleted after this call; the
    -- 'Right' means its nonce was incremented.
    iRealID :: ContractID,
    iDeleted :: Bool,
    iResult :: ReturnValue,
    iExportedResult :: ByteString,
    iVersions :: VersionMap,
    iOutputsM :: Maybe Outputs
  } deriving (Generic)

-- | The elements are 'Maybe' because an output contract may be deleted,
-- but the indexing of the others should not change, so we have to keep the
-- full array.
type Outputs = Vector Output

-- | Not only convenient, but also important for ensuring that the three
-- different source trees using this type all have the same version of it.
-- Since this type is exchanged in serialized form between different
-- processes, type checking cannot verify it at compile time.
type ExportData = (ContractID, Bool, [String], String, ByteString)

-- * Template Haskell

makeLenses ''TransactionEntry
makeLenses ''InputResults
makeLenses ''Storage

{- Instances -}

-- | For convenience, so we don't have to pattern-match elsewhere.
instance Show Result where
  show (Result x) = show x

-- * Functions

inputResultsAt :: TransactionID -> Int -> Lens' Storage (Maybe InputResults)
inputResultsAt txID ix = 
  _getStorage . 
  at txID .
  uncertain (txInputLens ix)

contractAtCID :: ContractID -> Lens' Storage (Maybe AbstractGlobalContract)
contractAtCID cID = outputAtNonce cID . lens getter setter where
  getter = fmap outputContract
  setter = flip . liftA2 $ set _outputContract 

-- | For the 'At' instance
type instance Index Storage = ContractID
-- | For the 'At' instance
type instance IxValue Storage = 
  Either (WithEscrows ReturnValue, VersionMap, Bool) OutputData
-- | For the 'At' instance
instance Ixed Storage
-- | We define this instance /in addition to/ the natural 'TransactionID'
-- indexing of a 'StorageT' so that we can look up contracts by ID, which
-- requires descending through several levels of lookups, any of which may
-- fail (in different ways).
instance At Storage where
  at cID = lens getter setter where
    getter st = 
      case st ^. outputAtNonce cID of
        Nothing -> Left <$> st ^. _importedValues . at cID
        x -> Right <$> x
    setter st (Just (Right x)) = st & outputAtNonce cID ?~ x
    setter st (Just (Left x)) = st & _importedValues . at cID ?~ x
    setter st Nothing = st
      & outputAtNonce cID .~ Nothing
      & _importedValues . at cID .~ Nothing

outputAtNonce :: ContractID -> Lens' Storage (Maybe OutputData)
outputAtNonce cID = 
  outputAt cID . 
  outputContractNonce (contractNonce cID)

outputAt :: ContractID -> Lens' Storage (Maybe Output)
outputAt cID@ContractID{..} =
  _getStorage .
  at parentTransaction .
  uncertain (txPartLens transactionPart) .
  uncertain (vectorAt creationIndex)

txPartLens :: TransactionPart -> Lens' TransactionEntry (Maybe Outputs)
txPartLens p = 
  case p of
    Body -> _outputs . onlyJust DeletedEntry
    InputCall n -> 
      txInputLens n . 
      uncertain _iOutputsM

txInputLens :: Int -> Lens' TransactionEntry (Maybe InputResults)
txInputLens n = _inputResults . vectorAt n

vectorAt :: Int -> Lens' (Vector a) (Maybe a)
vectorAt n = 
  onlyJust DeletedEntry .
  lens ((=<<) (Vector.!? n)) (\mv my -> mv >>= (my >>=) . setter)
 where
    setter v y
      | 0 <= n && n < Vector.length v = 
        Just $ Vector.modify (\w -> Vector.unsafeWrite w n y) v
      | otherwise = Nothing

outputContractNonce :: Nonce -> Lens' (Maybe Output) (Maybe OutputData) 
outputContractNonce nonce = lens getter setter . checkL where
  getter = (>>= outputData)
  setter mo mod = fmap (_outputData .~ ((_outputNonce +~ 1) <$> mod)) mo
  checkL | Current <- nonce = id
         | Nonce n <- nonce = guardNonce n

guardNonce :: (MonadPlus m) => Int -> Lens' (m OutputData) (m OutputData)
guardNonce n = (. (>>= g)) where
  g od@OutputData{..} = guard (n == outputNonce) >> return od

joinUncertainty :: 
  (Monad m) => Lens s (m (m t)) (m (m a)) (m b) -> Lens s (m t) (m a) (m b)
joinUncertainty = ((fmap join .) .) . (. (. join))

uncertain :: (Monad m) => Lens' s (m a) -> Lens' (m s) (m a)
uncertain l = lens getter setter where
  getter = (>>= view l)
  setter ms my = (l .~ my) <$> ms

onlyJust :: (Exception e) => e -> Lens' a (Maybe a)
onlyJust err = lens getter setter where
  getter = Just
  setter _ = fromMaybe $ throw err

listToOutputs :: [Output] -> Outputs
listToOutputs = Vector.fromList

-- | Deserializes an exported value as the correct type and puts it in
-- imported value storage for the future.  This is in `FaeStorage` and not
-- `FaeStorageT` because the former is not an instance of the latter, and
-- the interpreter benefits from having a monomorphic type.
addImportedValue :: 
  (Versionable a, HasEscrowIDs a, Exportable a) => 
  State Escrows a -> ContractID -> Bool -> FaeStorage ()
addImportedValue valueImporter cID deleted = FaeStorage $ do
  unless (hasNonce cID) $ throw $ ImportWithoutNonce cID
  let (importedValue, Escrows{..}) = 
        runState valueImporter (Escrows Map.empty nullDigest)
      vMap = versionMap (lookupWithEscrows escrowMap) importedValue
  _importedValues . at cID ?= 
    (WithEscrows escrowMap (ReturnValue importedValue), vMap, deleted)

getExportedValue :: (Monad m) => TransactionID -> Int -> FaeStorageT m ExportData
getExportedValue txID ix = FaeStorageT $ do
  InputResults{..} <- use $ inputResultsAt txID ix .
    defaultLens (throw $ BadInputID txID ix)
  Output{..} <- use $ outputAt iRealID . 
    defaultLens (throw $ BadContractID iRealID)
  let modNames = tyConModule <$> listTyCons outputType
  return (iRealID, iDeleted, modNames, show outputType, iExportedResult)

  where
    listTyCons :: TypeRep -> [TyCon]
    listTyCons rep = con : (reps >>= listTyCons) where
      (con, reps) = splitTyConApp rep


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
    importedValues :: Map ContractID (WithEscrows ReturnValue, VersionMap)
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
    iRealID :: ContractID,
    iResult :: !ReturnValue,
    iExportedResult :: ByteString,
    iVersions :: VersionRepMap,
    iOutputsM :: Maybe Outputs
  } deriving (Generic)

-- | The elements are 'Maybe' because an output contract may be deleted,
-- but the indexing of the others should not change, so we have to keep the
-- full array.
type Outputs = Vector (Maybe Output)

-- | Not only convenient, but also important for ensuring that the three
-- different source trees using this type all have the same version of it.
-- Since this type is exchanged in serialized form between different
-- processes, type checking cannot verify it at compile time.
type ExportData = (ContractID, [String], String, ByteString)

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
  txInputLens ix

-- | For the 'At' instance
type instance Index Storage = ContractID
-- | For the 'At' instance
type instance IxValue Storage = 
  Either (WithEscrows ReturnValue, VersionMap) AbstractGlobalContract
-- | For the 'At' instance
instance Ixed Storage
-- | We define this instance /in addition to/ the natural 'TransactionID'
-- indexing of a 'StorageT' so that we can look up contracts by ID, which
-- requires descending to various levels into the maps.
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

outputAtNonce :: ContractID -> Lens' Storage (Maybe AbstractGlobalContract)
outputAtNonce cID = 
  onlyJust (BadContractID cID) . 
  outputAt cID . 
  outputContractNonce (contractNonce cID)

outputAt :: ContractID -> Lens' (Maybe Storage) (Maybe Output)
outputAt ContractID{..} =
  uncertainA _getStorage .
  uncertainM1 (at parentTransaction) .
  txPartLens transactionPart .
  joinUncertainty (uncertainM1 $ updateVectorAt creationIndex)

txPartLens :: TransactionPart -> Lens' (Maybe TransactionEntry) (Maybe Outputs)
txPartLens Body = uncertainA _outputs
txPartLens (InputCall n) = txInputLens n . uncertainM1 _iOutputsM

txInputLens :: Int -> Lens' (Maybe TransactionEntry) (Maybe InputResults)
txInputLens n =
  uncertainA _inputResults . 
  uncertainM2 (updateVectorAt n)

updateVectorAt :: Int -> Lens (Vector a) (Maybe (Vector a)) (Maybe a) a
updateVectorAt n = lens (Vector.!? n) setter where
  setter v y 
    | 0 <= n && n < Vector.length v = 
      Just $ Vector.modify (\w -> Vector.unsafeWrite w n y) v
    | otherwise = Nothing

outputContractNonce :: 
  Nonce -> Lens' (Maybe Output) (Maybe AbstractGlobalContract) 
outputContractNonce nonce = checkL . lens getter setter where
  getter = fmap outputContract
  setter mo mc = (_outputNonce +~ 1) <$> liftM2 (_outputContract .~) mc mo
  checkL | Current <- nonce = id
         | Nonce n <- nonce = guardNonce n

guardNonce :: (MonadPlus m) => Int -> Lens' (m Output) (m Output)
guardNonce n = (. (>>= g)) where
  g o@Output{..} = guard (n == outputNonce) >> return o

joinUncertainty :: 
  (Monad m) => Lens s (m (m t)) (m (m a)) (m b) -> Lens s (m t) (m a) (m b)
joinUncertainty = ((fmap join .) .) . (. (. join))

uncertainM1 :: (Monad m) => Lens s t (m a) (m b) -> Lens (m s) (m t) (m a) (m b)
uncertainM1 l = lens getter setter where
  getter = (>>= myView l)
  setter ms my = (l .~ my) <$> ms

uncertainM2 :: (Monad m) => Lens s (m t) (m a) b -> Lens (m s) (m t) (m a) (m b)
uncertainM2 l = lens getter setter where
  getter = (>>= myView l)
  setter ms my = my >>= (ms >>=) . set l

uncertainA :: (Applicative m) => Lens s t a b -> Lens (m s) (m t) (m a) (m b)
uncertainA l = lens getter setter where
  getter = fmap $ myView l
  setter = flip $ liftA2 (set l)

-- | I have no idea why, but the 'view' from "Control.Lens" causes the
-- "uncertain" family of functions not to typecheck, whereas this works
-- fine.
myView :: Lens s t a b -> s -> a
myView l = getConst . l Const

onlyJust :: (Exception e) => e -> Lens' a (Maybe a)
onlyJust err = lens getter setter where
  getter = Just
  setter _ = fromMaybe $ throw err

listToOutputs :: [Output] -> Outputs
listToOutputs = Vector.fromList . map Just

-- | Deserializes an exported value as the correct type and puts it in
-- imported value storage for the future.  This is in `FaeStorage` and not
-- `FaeStorageT` because the former is not an instance of the latter, and
-- the interpreter benefits from having a monomorphic type.
addImportedValue :: 
  (Versionable a, HasEscrowIDs a, Exportable a) => 
  State Escrows a -> ContractID -> FaeStorage ()
addImportedValue valueImporter cID = FaeStorage $ do
  unless (hasNonce cID) $ throw $ ImportWithoutNonce cID
  let (importedValue, Escrows{..}) = 
        runState valueImporter (Escrows Map.empty nullDigest)
      vMap = versionMap (lookupWithEscrows escrowMap) importedValue
  _importedValues . at cID ?= 
    (WithEscrows escrowMap (ReturnValue importedValue), vMap)

getExportedValue :: (Monad m) => TransactionID -> Int -> FaeStorageT m ExportData
getExportedValue txID ix = FaeStorageT $ do
  InputResults{..} <- use $ inputResultsAt txID ix .
    defaultLens (throw $ BadInputID txID ix)
  Output{..} <- use $ outputAtID iRealID . 
    defaultLens (throw $ BadContractID iRealID)
  let modNames = tyConModule <$> listTyCons outputType
  return (iRealID, modNames, show outputType, iExportedResult)

  where
    listTyCons :: TypeRep -> [TyCon]
    listTyCons rep = con : (reps >>= listTyCons) where
      (con, reps) = splitTyConApp rep

outputAtID :: ContractID -> Lens' Storage (Maybe Output)
outputAtID cID = onlyJust (BadContractID cID) . outputAt cID



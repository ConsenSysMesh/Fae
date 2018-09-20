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
    result :: Result
  }

-- | The result can be anything, but should be 'show'able so that it has
-- outside meaning.  This is an existential type, so the record names are
-- just there for documentation; values have to be extracted by
-- pattern-matching.
data Result = forall a. (Show a) => Result a

-- | A minimal set of descriptors of the results of running a contract.
-- This used to contain an 'iVersions', but that can be derived from
-- 'iResult' now that it is 'WithEscrows'.
data InputResults =
  InputResults
  {
    -- | This used to be guaranteed to have a specific nonce (rather than
    -- 'Current') but this is no longer the case; it is exactly the
    -- 'ContractID' with which the call was made.
    iRealID :: ContractID,
    -- | Though 'Failed' corresponds to an exception being thrown from
    -- 'iResult', it is not possible to detect 'Deleted' from the other
    -- fields here, so this one is necessary.
    iStatus :: Status,
    -- | This is /very dangerous/ because these escrows are duplicates.
    -- This value /must not/ be used inside Fae, but only exposed to
    -- external applications (such as import/export).
    iResult :: WithEscrows ReturnValue,
    -- | 'Nothing' means that the results were imported and the contract
    -- wasn't actually run.  Perhaps in the future those can be imported
    -- too...
    iOutputsM :: Maybe Outputs
  } deriving (Generic)

-- | Semantic report of what happened in a contract call.  As such,
-- creating a 'Status' value requires a policy decision, made in
-- "Blockchain.Fae.Internal.Transaction".
data Status = Updated | Deleted | Failed deriving (Show, Generic)

-- | The elements are 'Maybe' because an output contract may be deleted,
-- but the indexing of the others should not change, so we have to keep the
-- full array.
type Outputs = Vector Output

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
instance Serialize ExportData

-- * Functions

-- ** 'Storage'-specific lenses

-- | Accessor partway down the hierarchy, stopping at the full
-- 'InputResults' and not checking the nonce.
inputResultsAt :: TransactionID -> Int -> Lens' Storage (Maybe InputResults)
inputResultsAt txID ix = 
  _getStorage . 
  at txID .
  uncertain (txInputLens ix)

-- | Simplified accessor for just the contract function, since often one
-- does not want to bother with the imported values or the nonce.  This
-- does check the nonce, however.
contractAtCID :: ContractID -> Lens' Storage (Maybe AbstractGlobalContract)
contractAtCID cID = outputAtNonce cID . lens getter setter where
  getter = fmap outputContract
  setter = flip . liftA2 $ set _outputContract 

-- | For the 'At' instance
type instance Index Storage = ContractID
-- | For the 'At' instance
type instance IxValue Storage = Either InputResults OutputData
-- | For the 'At' instance
instance Ixed Storage
-- | We define this instance /in addition to/ the natural 'TransactionID'
-- indexing of the 'getStorage' field so that we can look up contracts by
-- ID, which requires descending through several levels of lookups, any of
-- which may fail (in different ways).
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

-- | Accesses down to the output data without bothering with the imported
-- results, and checking the nonce.
outputAtNonce :: ContractID -> Lens' Storage (Maybe OutputData)
outputAtNonce cID = 
  outputAt cID . 
  outputContractNonce (contractNonce cID)

-- | Accesses the full 'Output' without checking the nonce (as makes
-- sense, as that is only available in the 'OutputData' below it).
outputAt :: ContractID -> Lens' Storage (Maybe Output)
outputAt cID@ContractID{..} =
  _getStorage .
  at parentTransaction .
  uncertain (txPartLens transactionPart) .
  uncertain (vectorAt creationIndex)

-- | Routes to the correct outputs list depending on where in the
-- transaction it happened.
txPartLens :: TransactionPart -> Lens' TransactionEntry (Maybe Outputs)
txPartLens p = 
  case p of
    Body -> _outputs . onlyJust DeletedEntry
    InputCall n -> 
      txInputLens n . 
      uncertain _iOutputsM

-- | Kind of redundant but used as a piece in several places, so worth
-- a named synonym.
txInputLens :: Int -> Lens' TransactionEntry (Maybe InputResults)
txInputLens n = _inputResults . vectorAt n

-- | This is a bit of a fake lens, as it slightly fails the law that
-- setting following getting is the identity; if you go out-of-bounds, the
-- former is an exception and the latter is, of course, a no-op.  Barring
-- that, this just marshals 'Vector.!?' and 'Vector.modify' to get and
-- set.
vectorAt :: Int -> Lens' (Vector a) (Maybe a)
vectorAt n = 
  onlyJust DeletedEntry .
  lens ((=<<) (Vector.!? n)) (\mv my -> mv >>= (my >>=) . setter)
 where
    setter v y
      | 0 <= n && n < Vector.length v = 
        Just $ Vector.modify (\w -> Vector.unsafeWrite w n y) v
      | otherwise = Nothing

-- | Descends into the 'OutputData' mindfully.  On getting, checks the
-- nonce (if one was specified); on setting, increments it (regardless).
outputContractNonce :: Nonce -> Lens' (Maybe Output) (Maybe OutputData) 
outputContractNonce nonce = lens getter setter . checkL where
  getter = (>>= outputData)
  setter mo mod = fmap (_outputData .~ ((_outputNonce +~ 1) <$> mod)) mo
  checkL | Current <- nonce = id
         | Nonce n <- nonce = guardNonce n

-- | Filters an 'OutputData' through a nonce check, but just for getting.
guardNonce :: (MonadPlus m) => Int -> Lens' (m OutputData) (m OutputData)
guardNonce n = (. (>>= g)) where
  g od@OutputData{..} = guard (n == outputNonce) >> return od

-- ** General lens combinators

-- | Basically, runs 'join' on the result of both a get and a set.
joinUncertainty :: 
  (Monad m) => Lens s (m (m t)) (m (m a)) (m b) -> Lens s (m t) (m a) (m b)
joinUncertainty = ((fmap join .) .) . (. (. join))

-- | Upgrades a lens that starts from a definite value to one that starts
-- from an "uncertain" (i.e. monadic) one.  Useful for chaining with 'at'.
uncertain :: (Monad m) => Lens' s (m a) -> Lens' (m s) (m a)
uncertain l = lens getter setter where
  getter = (>>= view l)
  setter ms my = (l .~ my) <$> ms

-- | A fake lens that is kind of the reverse of 'defaultLens'; this
-- purifies the result of a set at the expense of an exception.
onlyJust :: (Exception e) => e -> Lens' a (Maybe a)
onlyJust err = lens getter setter where
  getter = Just
  setter _ = fromMaybe $ throw err

-- ** Manipulating stored values

-- | Legacy synonym, though useful for hiding the implementation.
listToOutputs :: [Output] -> Outputs
listToOutputs = Vector.fromList

-- | Semantic interpretation of the status, hiding the pattern-match that
-- establishes the policy.
successful :: InputResults -> Bool
successful InputResults{..}
  | Failed <- iStatus = False
  | otherwise = True

-- | This function, formerly in 'acceptGlobal' from
-- "Blockchain.Fae.Internal.Contract", constructs the version map from the
-- basic results, following the policy that the result is null if the
-- contract ID didn't specify an exact nonce value. 
makeInputVersions :: InputResults -> VersionMap
makeInputVersions InputResults{iResult = WithEscrows{..},..} 
  | hasNonce iRealID = versionMap (lookupWithEscrows withEscrows) getWithEscrows
  | otherwise = emptyVersionMap

-- | Deserializes an exported value as the correct type and puts it in
-- imported value storage for the future.  This is in `FaeStorage` and not
-- `FaeStorageT` because the former is not an instance of the latter, and
-- the interpreter benefits from having a monomorphic type.
addImportedValue :: 
  (Versionable a, HasEscrowIDs a, Exportable a) => 
  State Escrows a -> ContractID -> Status -> FaeStorage ()
addImportedValue valueImporter iRealID iStatus = FaeStorage $ do
  unless (hasNonce iRealID) $ throw $ ImportWithoutNonce iRealID
  let (importedValue, Escrows{..}) = 
        runState valueImporter (Escrows Map.empty nullDigest)
      iVersions = versionMap (lookupWithEscrows escrowMap) importedValue
      iResult = WithEscrows escrowMap (ReturnValue importedValue)
  _importedValues . at iRealID ?= InputResults{iOutputsM = Nothing, ..}

-- | Converts the stored 'InputResults', which could actually have been the
-- result of a previous import, into the serialized return value plus other
-- metadata necessary for import elsewhere.
getExportedValue :: (Monad m) => TransactionID -> Int -> FaeStorageT m ExportData
getExportedValue txID ix = do
  InputResults{..} <- use $ inputResultsAt txID ix .
    defaultLens (throw $ BadInputID txID ix)
  unless (hasNonce iRealID) $ throw $ ExportWithoutNonce iRealID
  Output{..} <- use $ outputAt iRealID . 
    defaultLens (throw $ BadContractID iRealID)
  let neededModules = tyConModule <$> listTyCons outputType
      WithEscrows eMap result = iResult
      exportedValue = evalState (exportReturnValue result) eMap
  return ExportData 
    {
      exportedCID = iRealID,
      exportStatus = iStatus,
      exportNameType = show outputType,
      ..
    }

  where
    listTyCons :: TypeRep -> [TyCon]
    listTyCons rep = con : (reps >>= listTyCons) where
      (con, reps) = splitTyConApp rep

-- | Hides the particular structure of 'Storage' from other modules.
runStorageT :: (Monad m) => FaeStorageT m a -> m a
runStorageT = flip evalStateT $ Storage Map.empty Map.empty


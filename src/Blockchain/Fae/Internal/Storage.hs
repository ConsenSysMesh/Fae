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

import Control.Monad.IO.Class
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.Dynamic
import Data.Functor.Identity
import Data.IntMap (IntMap)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Serialize hiding (Result)

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import GHC.Generics (Generic)

import Type.Reflection

-- * Types

-- | 'Storage' records the complete effects of running each transaction.
-- It also holds any contract return values that have been imported; no
-- attempt is made to validate them against the actual return values, and
-- it is expected that users come to some agreement on their correctness
-- before sharing them.
data Storage = 
  Storage 
  { 
    getStorage :: Map TransactionID TransactionEntry ,
    importedValues :: Map ContractID (WithEscrows ReturnValue, VersionMap)
  }

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
    inputOutputs :: InputOutputs,
    inputOrder :: [ShortContractID],
    outputs :: Outputs,
    txSigners :: Signers,
    result :: Result
  }

-- | The result can be anything, but should be 'show'able so that it has
-- outside meaning.  This is an existential type, so the record names are
-- just there for documentation; values have to be extracted by
-- pattern-matching.
data Result = forall a. (Show a) => Result a

-- | Inputs are identified by 'ShortContractID's so that 'ContractIDs' of
-- the 'InputOutput' variant can be flat, rather than nested potentially
-- indefinitely.
type InputOutputs = Map ShortContractID InputResults
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
-- | Outputs are ordered by creation.  However, contracts can be deleted,
-- and deletion must preserve the original ordering index of the remaining
-- contracts, so it's not enough to just store them in a sequence.  In case
-- the outputs of several input calls need to be combined, the original
-- count of outputs needs to be retained.  The
-- second component of the map entries is the "nonce" of a contract, the
-- number of times it has been called, used for security.
data Outputs = 
  Outputs
  {
    outputMap :: IntMap ((AbstractGlobalContract, Int), SomeTypeRep),
    outputCount :: Int
  }

-- * Template Haskell

makeLenses ''TransactionEntry
makeLenses ''InputResults
makeLenses ''Outputs
makeLenses ''Storage

{- Instances -}

-- | Concatenates two output lists, shifting the second one by the full
-- count of the first one.
instance Semigroup Outputs where
  (<>) Outputs{outputMap = os1, outputCount = n1} 
       Outputs{outputMap = os2, outputCount = n2}
     = Outputs
       {
         outputMap = os1 <> os2',
         -- Verbose, but uses (<>) to emphasize the fact that we are
         -- building a 'Semigroup' instance.
         outputCount = getSum $ Sum n1 <> Sum n2
       }
     where os2' = IntMap.mapKeys (+ n1) os2

-- | -
instance Monoid Outputs where
  mempty = 
    Outputs
    {
      outputMap = IntMap.empty,
      outputCount = 0
    }
  mappend = (<>)

-- | Unions the outputs and the versions of the two arguments.  The failure
-- mode is when the arguments have a different 'iRealID', in which case we
-- choose the second one for the result.  This is because in "Transaction",
-- the second argument is the old entry, and we want to prevent people from
-- screwing up contract calls by way of subsequent calls.  This is
-- extraordinarily unlikely anyway, the way that the module works currently,
-- since to do so would require manufacturing a hash collision for the
-- short IDs.  If that happens, the later contract call is /definitely/
-- malicious, and so it's okay to mishandle it.
instance Semigroup InputResults where
  (<>) InputResults{iOutputsM = osM1, iVersions = VersionMap vs1}
       InputResults{iOutputsM = osM2, iVersions = VersionMap vs2, ..}
     = InputResults
       {
         -- 'combineO' shifts the second argument, which needs to be the new
         -- map.      
         iOutputsM = osM2 <> osM1, 
         iVersions = VersionMap $ vs1 <> vs2,
         ..
       }

-- | For convenience, so we don't have to pattern-match elsewhere.
instance Show Result where
  show (Result x) = show x

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
  at cID@((_ :# _) :# _) = throw $ InvalidContractID cID
  at (cID :# n) = valueWithNonce cID (Just n)
  at cID = valueWithNonce cID Nothing

-- * Functions

-- | Just combines 'nonceAt,' 'checkNonce', and 'elseImportedValue'.
valueWithNonce :: 
  ContractID -> Maybe Int -> Lens' Storage (Maybe (IxValue Storage))
valueWithNonce cID nM = lens getter setter where
  getter st = st ^. checkNonceAt cID nM . elseImportedValue st cID
  setter st Nothing = st
    & nonceAt cID .~ Nothing
    & _importedValues . at cID .~ Nothing
  setter st (Just (Right x)) = st & checkNonceAt cID nM ?~ x
  setter st (Just (Left x)) = st & _importedValues . at cID ?~ x

checkNonceAt :: 
  ContractID -> Maybe Int -> Lens' Storage (Maybe AbstractGlobalContract)
checkNonceAt cID nM = nonceAt cID . checkNonce cID nM

-- | Like 'at', but retaining the nonce
nonceAt :: ContractID -> Lens' Storage (Maybe (AbstractGlobalContract, Int))
nonceAt cID@(TransactionOutput txID i) =
  _getStorage .
  at txID .
  defaultLens (throw $ BadTransactionID txID) .
  _outputs .
  _outputMap .
  at i .
  lens (fmap fst) (liftM2 $ flip $ set _1)
nonceAt cID@(InputOutput txID sID i) = 
  _getStorage .
  at txID .
  defaultLens (throw $ BadTransactionID txID) .
  _inputOutputs .
  at sID .
  defaultLens (throw $ BadInputID txID sID) .
  _iOutputsM .
  defaultLens (throw $ BadInputID txID sID) .
  _outputMap .
  at i .
  lens (fmap fst) (liftM2 $ flip $ set _1)
nonceAt cID = throw $ InvalidNonceAt cID

-- | Enforces nonce-correctness when it is required
checkNonce :: ContractID -> Maybe Int -> Lens' (Maybe (a, Int)) (Maybe a)
checkNonce _ Nothing = lens (fmap fst) nonceSetter
checkNonce cID (Just n) = lens (fmap $ \(x, m) -> f m x) checkNonceSetter where
  checkNonceSetter xM@(Just (_, m)) yM = f m $ nonceSetter xM yM
  checkNonceSetter Nothing yM = nonceSetter Nothing yM
  f :: Int -> b -> b
  f m x = if m == n then x else throw $ BadNonce cID n m

-- | Updating a contract entry nonce-correctly
nonceSetter :: Maybe (a, Int) -> Maybe a -> Maybe (a, Int)
nonceSetter (Just (x, m)) (Just y) = Just (y, m + 1)
nonceSetter Nothing (Just y) = Just (y, 0)
nonceSetter _ _ = Nothing

-- | If no contract was found at this ID, try to get an imported return
-- value for it.
elseImportedValue :: 
  Storage -> ContractID -> 
  Getter (Maybe AbstractGlobalContract) (Maybe (IxValue Storage))
elseImportedValue st cID = to f where
  f Nothing = Left <$> st ^. _importedValues . at cID
  f x = Right <$> x

-- | Just an 'IntMap' constructor, indexing consecutively from 0.
listToOutputs :: OutputsList -> Outputs
listToOutputs l = 
  Outputs
  {
    outputMap = oMap, 
    outputCount = IntMap.size oMap
  }
  where oMap = IntMap.fromList $ zip [0 ..] $ zipWith f l (repeat 0) 
        f (rep, func) n = ((func, n), rep)

-- | Deserializes an exported value as the correct type and puts it in
-- imported value storage for the future.
addImportedValue :: 
  forall a m.
  (
    Versionable a, HasEscrowIDs a, Exportable a,
    MonadState Storage m
  ) => 
  ContractID -> TypeRep a -> ByteString -> m ()
addImportedValue cID rep bs = do
  let (importedValueM, Escrows{..}) = withTypeable rep $
        runState (importValue @a bs) (Escrows Map.empty nullDigest)
      importedValue = fromMaybe (throw $ CantImport bs $ SomeTypeRep rep) 
        importedValueM
      vMap = versionMap (lookupWithEscrows escrowMap) importedValue
  _importedValues . at cID ?= 
    (WithEscrows escrowMap (ReturnValue importedValue), vMap)


{- |
Module: Blockchain.Fae.Internal.Transaction
Description: Transaction execution
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides the code that navigates the intricacies of executing a transaction.  It is useful for front-end implementations, but not to users.
-}
module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.Monad.Writer

import Data.Foldable

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

-- * Types

-- | 'StorageT' is only parametrized because 'AbstractContract' isn't
-- defined in "Storage", which is because it would cause an import cycle.
-- Storage always contains abstract contracts.
type Storage = StorageT AbstractContract
-- | Likewise
type Outputs = OutputsT AbstractContract
-- | Likewise
type InputOutputs = InputOutputsT AbstractContract
-- | Likewise
type InputOutputVersions = InputOutputVersionsT AbstractContract
-- | Likewise
type FaeStorage = FaeStorageT AbstractContract
-- | Likewise
type TransactionEntry = TransactionEntryT AbstractContract

-- | How inputs are provided to transactions.
type Inputs = [(ContractID, String)]

-- | Transactions, though similar to contracts in many internal ways,
-- differ greatly in their inputs and outputs.  The argument of type 'a' is
-- constructed from the return values of the input contracts according to
-- its 'GetInputValues' instance.  The return value need not and can not
-- contain escrows (or rather, the escrow IDs it contains will not be
-- transferred anywhere).
type Transaction a b = a -> FaeM Naught b

-- | This monad is a little different from 'FaeTX' in that it is built on
-- 'FaeStorage', meaning it has direct access to the storage and also to
-- 'IO'.  Needless to say, we don't want any of that near user-defined
-- contracts.
type TXStorageM = FaeContractT Naught FaeStorage

-- * Functions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.
runTransaction :: 
  forall a inputs.
  (
    GetInputValues inputs, HasEscrowIDs inputs, Typeable inputs, 
    Typeable a, Show a
  ) =>
  Transaction inputs a -> [Transaction inputs ()] -> 
  Inputs -> TransactionID -> Signers -> Bool ->
  FaeStorage ()
runTransaction f fallback inputArgs txID signers isReward = 
 runFaeContract txID signers $ do -- TXStorageM
  let 
    inputIDs = map fst inputArgs
    defaultIOs = flip map inputIDs $ \cID ->
      (shorten cID, InputOutputVersions (withoutNonce cID) IntMap.empty Map.empty)
    inputOrder = map fst defaultIOs
  liftFaeContract $ txStorage ?= 
    TransactionEntry
    {
      inputOutputs = Map.fromList defaultIOs,
      inputOrder = inputOrder,
      outputs = IntMap.empty,
      signers,
      result = undefined :: a
    }
  -- Modifies inputOutputs
  inputsL <- runInputContracts inputArgs
  (result, outputs) <- doTX inputsL fallback isReward f
  liftFaeContract $ txStorage %= 
    -- Keep inputOutputs, set result and outputs
    -- Can't use a record update because of existential quantification
    fmap (\TransactionEntry{inputOutputs} -> TransactionEntry{..})

  where txStorage = _getStorage . at txID
 
doTX :: 
  (HasEscrowIDs inputs, GetInputValues inputs, Typeable inputs) => 
  [BearsValue] -> [Transaction inputs ()] -> Bool -> 
  Transaction inputs a -> TXStorageM (a, Outputs)
doTX inputsL fallback isReward f = 
  -- We have to handle exceptions because we don't want the inputs to get
  -- rolled back.
  handleAll (\e -> return (throw e, throw e)) $ do
    (input, unused) <- getInputValues <$> withReward inputsL
    unless (null unused) $ throw TooManyInputs
    -- We have to hoist so that transactions can be pure
    (result, outputsL) <- listen $ catchAll 
      (hoistFaeContract $! getFae $ f input)
      (\e -> doFallback fallback input >> return (throw e))
    censor (const []) $ return (result, intMapList outputsL)

  where
    withReward 
      | isReward = \inputsL -> do
          eID <- internalNewEscrow [] $ \Token -> internalSpend Reward
          return $ bearer eID : inputsL
      | otherwise = return 

-- | Performs all fallback transactions, ignoring errors.
doFallback :: [Transaction inputs ()] -> inputs -> TXStorageM ()
doFallback fallback input = forM_ fallback $ 
  \tx -> handleAll (const $ return ()) $ hoistFaeContract $ getFae $ tx input

-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts :: Inputs -> TXStorageM [BearsValue]
runInputContracts = fmap fst .
  foldl' (>>=) (return ([], emptyVersionMap)) .
  map (uncurry runInputContract) 

-- | Runs a single input contract.
runInputContract ::
  ContractID -> String ->
  ([BearsValue], VersionMap) -> TXStorageM ([BearsValue], VersionMap)
runInputContract cID arg (results, vers) = do
  let inputError e = return (throw e, vers, throw e)
  (result, vers', ioV) <- handleAll inputError $ do
    ConcreteContract fAbs <- liftFaeContract $ use $
      at cID . defaultLens (throw $ BadInput cID)
    -- This strictness is /so/ important.  It flushes out errors that would
    -- otherwise be saved in future iterations of the contract.
    ((!gAbsM, (!result, !vMap)), !outputsL) <- listen $ fAbs (arg, vers)
    let 
      iRealID = withoutNonce cID
      iOutputs = intMapList outputsL
      -- Only nonce-protected contract calls are allowed to return
      -- versioned values.
      (iVersions, vers')
        | hasNonce cID = (fmap bearerType $ getVersionMap vMap, vers `vUnion` vMap)
        | otherwise = (Map.empty, vers)
    liftFaeContract $ at cID .= gAbsM
    return (result, vers', InputOutputVersions{..})
  -- Nothing after this line should ever throw an exception
  txID <- view _thisTXID
  liftFaeContract $ _getStorage . at txID %= 
    fmap (_inputOutputs . at (shorten cID) ?~ ioV)
  censor (const []) $ return (results |> result, vers')


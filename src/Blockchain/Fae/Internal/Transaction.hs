{- |
Module: Blockchain.Fae.Internal.Transaction
Description: Transaction execution
Copyright: (c) Ryan Reich, 2017-2018
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
import Blockchain.Fae.Internal.NFData
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable
import Data.Functor.Identity

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
type FaeStorageM m = FaeStorageT m AbstractContract
-- | Likewise
newtype FaeStorage a = FaeStorage { getFaeStorage :: FaeStorageM Identity a }
-- | Likewise
type TransactionEntry = TransactionEntryT AbstractContract
-- | Likewise
type ExplicitContract = 
  ContractF Naught Identity (String, VersionMap') (BearsValue, VersionMap) 

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
type TXStorageM = FaeContractT Naught (FaeStorageM Identity)

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
runTransaction f fallback inputArgs txID signers isReward = FaeStorage $ do
  txStorage ?= 
    TransactionEntry
    {
      inputOutputs = InputOutputs $ Map.fromList defaultIOs,
      inputOrder = inputOrder,
      outputs = emptyOutputs,
      signers,
      result = Result (undefined :: a)
    }
  ~(result, outputs) <- runFaeContract txID signers $ do
    inputsL <- runInputContracts inputArgs
    hoistFaeContract $ doTX inputsL fallback isReward f
  txStorage %= fmap (\txE -> txE{result, outputs})

  where 
    txStorage = _getStorage . at txID
    inputIDs = map fst inputArgs
    defaultIOs = flip map inputIDs $ \cID ->
      (
        shorten cID, 
        InputOutputVersions (withoutNonce cID) emptyOutputs emptyVersionMap
      )
    inputOrder = map fst defaultIOs
 
-- | Actually perform the transaction
doTX :: 
  (HasEscrowIDs inputs, GetInputValues inputs, Typeable inputs, Show a) => 
  [BearsValue] -> [Transaction inputs ()] -> Bool -> 
  Transaction inputs a -> FaeContract Naught (Result, Outputs)
doTX inputsL fallbacks isReward f = do
  (input, unused) <- getInputValues <$> withReward inputsL
  ~(result, outputsL) <- lazify $ -- In case the conditional itself throws
    if null unused
    then listen $ do
      ~(result, outputsL) <- listen $ lazify $ getFae $ f input
      let successful = unsafeIsDefined $ outputsL `deepseq` result
      unless successful $ do
        -- If the transaction fails, it has to be rolled back.  Also, this
        -- ensures that this failure can't pollute the outputs that the
        -- fallbacks might create
        censor (const []) $ return ()
        doFallback fallbacks input
      return result
    else return $ throw TooManyInputs
  censor (const []) $ return (Result result, listToOutputs outputsL)

  where
    withReward inputsL
      | isReward = do
          eID <- internalNewEscrow [] $ \Token -> internalSpend Reward
          return $ bearer eID : inputsL
      | otherwise = return inputsL

-- | Performs all fallback transactions, ignoring errors.
doFallback :: [Transaction inputs ()] -> inputs -> FaeContract Naught ()
doFallback fallbacks input = forM_ fallbacks $ \tx -> do
  -- input is already forced from the main transaction
  result <- lazify $ getFae $ tx input 
  let 
    successful = unsafeIsDefined result
    act = if successful then id else censor (const [])
  act $ return ()

-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts :: Inputs -> TXStorageM [BearsValue]
runInputContracts = fmap fst .
  foldl' (>>=) (return ([], emptyVersionMap')) .
  map (uncurry runInputContract) 

-- | Runs a single input contract as a monadic state update, adding the
-- result to the ongoing list and updating the map of all defined versions.
runInputContract ::
  ContractID -> String ->
  ([BearsValue], VersionMap') -> TXStorageM ([BearsValue], VersionMap')
runInputContract cID arg (results, vers) = do
  escrows <- get
  ~(ConcreteContract fAbs) <- liftFaeContract $ use $
    at cID . defaultLens (throw $ BadInput cID)
  ~(result, ioV, vers', gAbsM) <- 
    hoistFaeContractNaught $ lazify $ runContract cID vers fAbs arg
  -- Success means that the result and continuation are defined.
  -- Because of the internal use of 'lazify', this also includes the
  -- escrows and outputs.
  let 
    testVal = gAbsM `deepseq` result
    ioV' = testVal `seq` ioV
    successful = unsafeIsDefined testVal
  when successful $ liftFaeContract $ at cID .= gAbsM
  txID <- view _thisTXID
  liftFaeContract $ _getStorage . at txID %= 
    -- Important here that 'ioV', the new versions, are the /first/
    -- argument to 'combineIOV'; see the comments for that function.
    fmap (_inputOutputs . _getInputOutputs . at (shorten cID) %~ 
      Just . maybe ioV' (combineIOV ioV'))
  return (results |> result, vers')

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  VersionMap' ->
  ExplicitContract -> 
  String ->
  FaeContract Naught 
    (BearsValue, InputOutputVersions, VersionMap', 
    Maybe AbstractContract)
runContract cID vers fAbs arg = do
  ((gAbsM, (result, vMap)), outputsL) <- listen $ fAbs (arg, vers)
  let 
    iRealID = withoutNonce cID
    iOutputs = listToOutputs outputsL
    -- Only nonce-protected contract calls are allowed to return
    -- versioned values.
    (iVersions, vers')
      | hasNonce cID = 
          (
            VersionMap $ bearerType <$> getVersionMap vMap, 
            addContractVersions cID vMap vers
          )
      | otherwise = (emptyVersionMap, vers)
  censor (const []) $ return (result, InputOutputVersions{..}, vers', gAbsM)

-- | Boosts the base monad from 'Identity'.
hoistFaeContractNaught :: 
  (Monad m) => FaeContract Naught a -> FaeContractT Naught m a
hoistFaeContractNaught = mapMonadNaught hoistFaeRWST


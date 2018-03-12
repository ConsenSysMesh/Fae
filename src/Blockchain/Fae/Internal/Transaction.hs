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

import qualified Control.DeepSeq as DS

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
doTX inputsL fallback isReward f = do
  ~(input, unused) <- getInputValues <$> withReward inputsL
  ~(result, outputsL) <- 
    if null unused -- unused /must/ always be defined
    then listen $ getFae $ f $ force input
    else return $ throw TooManyInputs
    -- Still have to figure out how to do the fallbacks without catching
    --    (\e -> doFallback fallback input >> return (throw e))
  censor (const []) $ return (Result result, listToOutputs outputsL)

  where
    withReward 
      | isReward = \inputsL -> do
          eID <- internalNewEscrow [] $ \Token -> internalSpend Reward
          return $ bearer eID : inputsL
      | otherwise = return 

-- | Performs all fallback transactions, ignoring errors.
--doFallback :: [Transaction inputs ()] -> inputs -> TXStorageM ()
--doFallback fallback input = forM_ fallback $ 
--  \tx -> handleAll (const $ return ()) $ hoistFaeContract $ getFae $ tx input

-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts :: Inputs -> TXStorageM [BearsValue]
runInputContracts = fmap fst .
  foldl' (>>=) (return ([], emptyVersionMap')) .
  map (uncurry runInputContract) 

-- | Runs a single input contract.
runInputContract ::
  ContractID -> String ->
  ([BearsValue], VersionMap') -> TXStorageM ([BearsValue], VersionMap')
runInputContract cID arg (results, vers) = do
  ~(result, vers', ioV) <- do
    ConcreteContract fAbs <- liftFaeContract $ use $
      at cID . defaultLens (throw $ BadInput cID)
    ~(result, ioV, vers', gAbsM) <- 
      hoistFaeContractNaught $ runContract cID vers fAbs arg
    liftFaeContract $ at cID .= gAbsM
    return (result, vers', ioV)
  -- Nothing after this line should ever throw an exception
  txID <- view _thisTXID
  liftFaeContract $ _getStorage . at txID %= 
    -- Important here that 'ioV', the new versions, are the /first/
    -- argument to 'combineIOV'; see the comments for that function.
    fmap (_inputOutputs . _getInputOutputs . at (shorten cID) %~ 
      Just . maybe ioV (combineIOV ioV))
  censor (const []) $ return (results |> result, vers')

runContract ::
  ContractID ->
  VersionMap' ->
  ExplicitContract -> 
  String ->
  FaeContract Naught 
    (BearsValue, InputOutputVersions, VersionMap', 
    Maybe AbstractContract)
runContract cID vers fAbs arg = do
  ~(~(gAbsM, ~(result, vMap)), outputsL) <- listen $ lazify $ fAbs (arg, vers)
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
  return (result, InputOutputVersions{..}, vers', DS.force gAbsM)

-- | Boosts the base monad from 'Identity'.
hoistFaeContractNaught :: 
  (Monad m) => FaeContract Naught a -> FaeContractT Naught m a
hoistFaeContractNaught = mapMonadNaught hoistFaeRWST

-- | It's possible that the monad itself, and not just the value it
-- "contains", is bottom.  This function lazily unwraps the monad, pushing
-- the bottom into the contained value.
lazify :: FaeContract Naught a -> FaeContract Naught a
lazify (Coroutine eithM) = Coroutine $ lazifyS eithM where
  lazifyS (StateT f) = StateT $ lazifyW . f
  lazifyW (WriterT p) = WriterT $ lazifyR p
  lazifyR (ReaderT f) = ReaderT $ lazifyX . f
  lazifyX (Identity ~(~(~(Right x), s), w)) = Identity ((Right x, s), w)


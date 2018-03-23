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
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Control.DeepSeq

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor
import Data.Foldable
import Data.Functor.Identity

import qualified Data.Map as Map

-- * Types

-- | How inputs are provided to transactions.
type Inputs = [(ContractID, String)]

-- | The version used for running pure transactions
newtype FaeStorage a = FaeStorage { getFaeStorage :: FaeStorageT Identity a }
-- | Transaction monad with storage access, for executing contracts and
-- saving the results.
type TXStorageM = StateT Storage FaeTXM
-- | A general storage transformer; used for running transactions in IO.
type FaeStorageT = StateT Storage

-- * Functions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.
runTransaction :: 
  forall inputs a.
  (GetInputValues inputs, HasEscrowIDs inputs, Typeable a, Show a) =>
  Transaction inputs a -> [Transaction inputs ()] -> 
  Inputs -> TransactionID -> Signers -> Bool ->
  FaeStorage ()
runTransaction f fallback inputArgs txID txSigners isReward = FaeStorage $ do
  txStorage ?= 
    TransactionEntry
    {
      inputOutputs = Map.fromList defaultIOs,
      inputOrder = inputOrder,
      outputs = emptyOutputs,
      txSigners,
      result = Result (undefined :: a)
    }
  ~(result, outputs) <- runTX $ do
    inputsL <- runInputContracts inputArgs
    lift $ doTX inputsL fallback isReward f
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
    runTX = mapStateT $ 
      fmap fst . runWriterT . flip runReaderT txData . flip evalStateT escrows
    txData = TXData{ thisTXSigners = txSigners, thisTXID = txID }
    escrows = Escrows { escrowMap = Map.empty, nextID }
    ShortContractID nextID = txID
 
-- | Actually perform the transaction
doTX :: 
  (HasEscrowIDs inputs, GetInputValues inputs, Show a) => 
  [BearsValue] -> [Transaction inputs ()] -> Bool -> 
  Transaction inputs a -> FaeTXM (Result, Outputs)
doTX inputsL fallbacks isReward f = do
  ~(input, unused) <- getInputValues <$> withReward inputsL
  if null unused
  then fmap (bimap Result listToOutputs) . listen $ do
    result <- getFae $ f input
    unless (unsafeIsDefined result) $ do
      -- If the transaction fails, it has to be rolled back.
      censor (const []) $ return ()
      doFallback fallbacks input
  else return $ throw TooManyInputs

  where
    withReward inputsL
      | isReward = do
          eID <- getFae $ newEscrow [] $ \Token -> spend Reward
          return $ bearer eID : inputsL
      | otherwise = return inputsL

-- | Performs all fallback transactions, ignoring errors.
doFallback :: [Transaction inputs ()] -> inputs -> FaeTXM ()
doFallback fallbacks input = forM_ fallbacks $ \tx -> do
  result <- getFae $ tx input 
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
  ~(result, ioV, vers', gAbsM) <- do
    fAbs <- use $ at cID . defaultLens (throw $ BadInput cID)
    lift $ runContract cID vers fAbs arg
  -- Success means that the result and continuation are defined.
  let 
    testVal = gAbsM `deepseq` result
    ioV' = testVal `seq` ioV
    successful = unsafeIsDefined testVal
  when successful $ at cID .= gAbsM
  txID <- view _thisTXID
  -- Important here that 'ioV', the new versions, are the /first/
  -- argument to 'combineIOV'; see the comments for that function.
  _getStorage . at txID %= 
    fmap (_inputOutputs . at (shorten cID) %~ Just . maybe ioV' (combineIOV ioV'))
  return (results |> result, vers')

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  VersionMap' ->
  AbstractGlobalContract -> 
  String ->
  FaeTXM 
    (BearsValue, InputOutputVersions, VersionMap', Maybe AbstractGlobalContract)
runContract cID vers fAbs arg = do
  ~(~(~(result, vMap), gAbsM), outputsL) <- listen $ callContract fAbs (arg, vers)
  let 
    iRealID = withoutNonce cID
    iOutputs = listToOutputs outputsL
    -- Only nonce-protected contract calls are allowed to return
    -- versioned values.
    ~(iVersions, vers')
      | hasNonce cID = 
          (
            VersionMap $ bearerType <$> getVersionMap vMap, 
            addContractVersions cID vMap vers
          )
      | otherwise = (emptyVersionMap, vers)
  censor (const []) $ return (result, InputOutputVersions{..}, vers', gAbsM)

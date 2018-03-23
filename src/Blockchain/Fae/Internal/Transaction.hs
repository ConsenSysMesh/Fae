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

-- ** Running transactions

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
  ~(result, outputs) <- runTX $ runInputContracts inputArgs >>= 
    lift . (withReward >=> doTX f fallback . getInputValues)
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

    withReward inputsL
      | isReward = do
          eID <- getFae $ newEscrow [] $ \Token -> spend Reward
          return $ bearer eID : inputsL
      | otherwise = return inputsL
 
-- | Actually perform the transaction.
doTX :: 
  (HasEscrowIDs inputs, GetInputValues inputs, Show a) => 
  Transaction inputs a -> [Transaction inputs ()] -> 
  inputs -> FaeTXM (Result, Outputs)
doTX f fallbacks x = 
  (_2 %~ listToOutputs . force) <$> callTX (doFallbacks fallbacks) x f

-- | Performs all fallback transactions, ignoring errors.
doFallbacks :: [Transaction inputs ()] -> inputs -> FaeTXM OutputsList
doFallbacks fallbacks x = fmap concat . 
  forM fallbacks $ fmap snd . callTX (const $ return []) x

-- | Calls the transaction function, keeping the metadata safe.  Escrows
-- are saved in advance and restored before running the fallback function,
-- and errors in the call itself or its outputs are deferred to the return
-- value.
callTX :: 
  (Show a) =>
  (inputs -> FaeTXM OutputsList) -> inputs -> 
  Transaction inputs a -> FaeTXM (Result, OutputsList)
callTX g x f = do
  escrows <- get
  let fallback = put escrows >> g x
  ~(y, w) <- listen . getFae $ f x
  let result = w `deepseq` Result y
  (result,) <$> if unsafeIsDefined result then return w else fallback

-- ** Running contracts

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

  -- We don't update the contract if it didn't return cleanly
  let successful = unsafeIsDefined result
  when successful $ at cID .= gAbsM

  txID <- view _thisTXID
  -- Important here that 'ioV', the new versions, are the /first/
  -- argument to 'combineIOV'; see the comments for that function.
  _getStorage . at txID %= 
    fmap (_inputOutputs . at (shorten cID) %~ Just . maybe ioV (combineIOV ioV))
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
  let 
    -- The actual result of the contract includes both 'result' and also
    -- the outputs and continuation function, so we link their fates.
    result' = gAbsM `deepseq` outputsL `seq` result
    iov = result' `seq` InputOutputVersions{..} 
  return (result', iov, vers', gAbsM)


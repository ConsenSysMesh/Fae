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
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.GenericInstances
import Blockchain.Fae.Internal.GetInputValues
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Versions

import Common.Lens

import Control.DeepSeq

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))

import Data.Foldable
import Data.Maybe
import Data.Semigroup ((<>))

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.Map as Map
import Data.Map (Map)

-- * Types

-- | How inputs are provided to transactions.  The contract with a given ID
-- gets an argument and a (possibly empty) remapping of signer names.
type Inputs = [(ContractID, String, Renames)]

-- | Transaction monad with storage access, for executing contracts and
-- saving the results.
type TXStorageM = StateT Storage FaeTXM

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
      inputResults = 
        Vector.fromList $ map (defaultInputResult . view _1) inputArgs,
      outputs = mempty,
      txSigners,
      result = Result (undefined :: a)
    }
  ~(result, outputs) <- runTX $ runInputContracts inputArgs >>= 
    lift . (withReward >=> doTX f fallback . getInputValues)
  txStorage %= fmap (\txE -> txE{result, outputs})

  where 
    txStorage = _getStorage . at txID
    defaultInputResult cID = 
      InputResults
      {
        iRealID = cID,
        iDeleted = False,
        iResult = ReturnValue (),
        iExportedResult = mempty,
        iVersions = emptyVersionMap,
        iOutputsM = mempty
      }
    
    runTX = mapStateT $ 
      fmap fst . runWriterT . flip runReaderT txData . flip evalStateT escrows
    txData = 
      TXData
      {
        thisTXSigners = txSigners,
        localHash = txID,
        thisTXID = txID
      }
    escrows = Escrows { escrowMap = Map.empty, nextID = txID }

    withReward inputsL
      | isReward = do
          eID <- getFaeTX $ newEscrow RewardName
          return $ ReturnValue (Reward eID) : inputsL
      | otherwise = return inputsL
 
-- | Actually perform the transaction.
doTX :: 
  (HasEscrowIDs inputs, GetInputValues inputs, Show a) => 
  Transaction inputs a -> [Transaction inputs ()] -> 
  inputs -> FaeTXM (Result, Outputs)
doTX f fallbacks x = do
  txID <- view _thisTXID
  (_2 %~ listToOutputs . force) <$> callTX (doFallbacks fallbacks) x f

-- | Performs all fallback transactions, ignoring errors.
doFallbacks :: [Transaction inputs ()] -> inputs -> FaeTXM [Output]
doFallbacks fallbacks x = fmap concat . 
  forM fallbacks $ fmap snd . callTX (const $ return []) x

-- | Calls the transaction function, keeping the metadata safe.  Escrows
-- are saved in advance and restored before running the fallback function,
-- and errors in the call itself or its outputs are deferred to the return
-- value.
callTX :: 
  (Show a) =>
  (inputs -> FaeTXM [Output]) -> inputs -> 
  Transaction inputs a -> FaeTXM (Result, [Output])
callTX g x f = do
  escrows <- get
  let fallback = put escrows >> g x
  ~(y, w) <- listen . getFaeTX $ f x
  let result = w `deepseq` Result y
  (result,) <$> if unsafeIsDefined result then return w else fallback

-- ** Running contracts

-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts :: Inputs -> TXStorageM [ReturnValue]
runInputContracts inputs = fmap fst $
  foldl' (>>=) (return ([], emptyInputVersionMap $ length inputs)) $
    zipWith nextInput [0 ..] inputs

-- | If the contract function is available, run it as a monadic state
-- update, adding the result to the ongoing list and updating the map of
-- all defined versions.  If instead we have an imported value, just use
-- that.  Otherwise, this input is exceptional (and so, probably, is the
-- transaction result).
nextInput ::
  Int -> (ContractID, String, Renames) -> 
  ([ReturnValue], InputVersionMap) -> 
  TXStorageM ([ReturnValue], InputVersionMap)
nextInput ix (cID, arg, Renames renames) (results, vers) = do
  valEM <- use $ at cID 

  -- Lazy because 'Nothing' throws an exception.
  ~iR@InputResults{..} <- case valEM of
    -- A quick way of assigning the same exception to all the fields of the
    -- 'InputResults', but setting the 'iRealID' and 'iDeleted' to actual
    -- values, which are useful even if the contract itself is missing.
    Nothing -> return $ (throw $ BadContractID cID) & 
      \ ~InputResults{..} -> InputResults{iRealID = cID, iDeleted = False, ..}
    Just (Right OutputData{..}) -> do 
      let cID' = cID{contractNonce = Nonce outputNonce}
      ~(iR, gAbsM) <- lift $ 
        local (_localHash .~ digest arg) . remapSigners renames $ 
          runContract cID' vers outputContract arg

      -- We don't update the contract if it didn't return cleanly
      let successful = unsafeIsDefined iR
      when successful $ contractAtCID cID' .= gAbsM

      return iR

    Just (Left (x, iVersions, iDeleted)) -> do
      at cID .= Nothing -- Just to make sure
      iResult <- lift $ putEscrows x
      iExportedResult <- lift $ exportReturnValue iResult
      return InputResults{iRealID = cID, iOutputsM = Nothing, ..}

  txID <- view _thisTXID
  _getStorage . at txID . uncertain (txInputLens ix) ?= iR 
  return (results |> iResult, addContractVersions ix iVersions vers)

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.  The 'ContractID' argument must have a 'Nonce
-- Int' and not a 'Current' nonce.
runContract ::
  ContractID ->
  InputVersionMap ->
  AbstractGlobalContract -> 
  String ->
  FaeTXM (InputResults, Maybe AbstractGlobalContract)
runContract iRealID vers fAbs arg = do
  thisTXID <- view _thisTXID
  ~(~(~(result, iVersions), gAbsM), outputsL) <- 
    listen $ callContract fAbs (arg, vers)
  let 
    -- We store this with the nonce (if given) so that if (and when) it is
    -- exported, the correct id-with-nonce can be retrieved.  Once the
    -- contract is called again, its current nonce is no longer the correct
    -- one for this result.  The 'Either' syntax indicates whether the
    -- contract was deleted or updated.
    iDeleted = isNothing gAbsM
    iOutputsM = Just $ listToOutputs outputsL
  -- The actual result of the contract includes both 'result' and also
  -- the outputs and continuation function, so we link their fates.
  let iResult = gAbsM `deepseq` outputsL `seq` result
  iExportedResult <- exportReturnValue iResult
  return (InputResults{..}, gAbsM)


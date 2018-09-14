{- |
Module: Blockchain.Fae.Internal.Transaction
Description: Transaction execution
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides the code that navigates the intricacies of executing a transaction.  It is useful for front-end implementations, but not to users.
-}
{-# LANGUAGE DataKinds #-}
module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
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
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Typeable

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

-- * Typeclasses

-- | This class, and its only instances, define what a transaction function
-- must look like: essentially, any function of the form
--
-- >>> body :: a -> b -> ... -> FaeTX result
--
-- where the argument types correspond, in order, to the return types of
-- the transaction's input contracts.  Correspondingly, any fallback
-- functions must have the signature
--
-- >>> fallback :: a -> b -> ... -> FaeTX ()
--
-- though other return value types are accepted, and ignored.
class 
  (Typeable (TransactionResult f), Show (TransactionResult f)) =>
  TransactionBody f where

  type TransactionResult f :: *
  type TransactionFallback f :: *
  callTransactionBody :: f -> [ReturnValue] -> FaeTX (TransactionResult f)

-- | A little more concise than its expansion.
type TransactionConstraints f =
  (TransactionBody f, TransactionBody (TransactionFallback f))

{- Instances -}

-- | The base-case instance: a transaction with no arguments.
instance (Typeable a, Show a) => TransactionBody (FaeTX a) where
  type TransactionResult (FaeTX a) = a
  type TransactionFallback (FaeTX a) = FaeTX ()
  callTransactionBody f [] = f
  callTransactionBody _ _ = throw TooManyInputs

-- | The inductive-case instance: a transaction with one more argument.
instance (TransactionBody f, Typeable a) => TransactionBody (a -> f) where
  type TransactionResult (a -> f) = TransactionResult f
  type TransactionFallback (a -> f) = a -> TransactionFallback f
  callTransactionBody g [] = throw NotEnoughInputs
  callTransactionBody g (x : rest) = callTransactionBody (g $ cast x) rest
    where 
      cast rv = getReturnValue rv $
        throw $ BadArgType (returnValueType rv) (typeRep $ Proxy @a)

-- * Functions

-- ** Running transactions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.
runTransaction :: 
  (TransactionConstraints f) =>
  f -> [TransactionFallback f] -> 
  Inputs -> TransactionID -> Signers -> Bool -> FaeStorage ()
runTransaction f fallback inputArgs txID txSigners isReward = FaeStorage $ do
  txStorage ?= 
    TransactionEntry
    {
      inputResults = 
        Vector.fromList $ map (defaultInputResult . view _1) inputArgs,
      outputs = mempty,
      txSigners,
      result = throw $ IncompleteTransaction txID
    }
  ~(result, outputs) <- runTX $ runInputContracts inputArgs >>= 
    lift . (withReward >=> doTX f fallback)
  txStorage %= fmap (\txE -> txE{result, outputs})

  where 
    txStorage = _getStorage . at txID
    defaultInputResult cID = 
      InputResults
      {
        iRealID = cID,
        iStatus = Failed,
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
  (TransactionConstraints f) => 
  f -> [TransactionFallback f] -> [ReturnValue] -> FaeTXM (Result, Outputs)
doTX f fallbacks x = do
  txID <- view _thisTXID
  (_2 %~ listToOutputs . force) <$> callTX (doFallbacks fallbacks) x f

-- | Performs all fallback transactions, ignoring return values and errors.
doFallbacks :: (TransactionBody g) => [g] -> [ReturnValue] -> FaeTXM [Output]
doFallbacks fallbacks x = fmap concat . 
  forM fallbacks $ fmap snd . callTX (const $ return []) x

-- | Calls the transaction function, keeping the metadata safe.  Escrows
-- are saved in advance and restored before running the fallback function,
-- and errors in the call itself or its outputs are deferred to the return
-- value.
callTX :: 
  (TransactionBody f) => 
  ([ReturnValue] -> FaeTXM [Output]) -> [ReturnValue] -> f ->
  FaeTXM (Result, [Output])
callTX g x f = do
  escrows <- get
  let fallback = put escrows >> g x
  ~(y, w) <- listen . getFaeTX $ callTransactionBody f x
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
    -- 'InputResults', but setting the 'iRealID' and 'iStatus' to actual
    -- values, which are useful even if the contract itself is missing.
    Nothing -> return $ (throw $ BadContractID cID) & 
      \ ~InputResults{..} -> InputResults{iRealID = cID, iStatus = Failed, ..}
    Just (Right OutputData{..}) -> do 
      ~(iR, gAbsM) <- lift $ 
        local (_localHash .~ digest arg) . remapSigners renames $ 
          runContract cID vers outputContract arg

      -- We don't update the contract if it didn't return cleanly
      when (successful iR) $ contractAtCID cID .= gAbsM

      -- The nonce needs to be made explicit for the purpose of exporting
      -- the return value with correct information of when it applies.
      return $ iR & _iRealID . _contractNonce .~ Nonce outputNonce

    Just (Left (x, iVersions, iStatus)) -> do
      at cID .= Nothing -- Just to make sure
      iResult <- lift $ putEscrows x
      iExportedResult <- lift $ exportReturnValue iResult
      return InputResults{iRealID = cID, iOutputsM = Nothing, ..}

  txID <- view _thisTXID
  _getStorage . at txID . uncertain (txInputLens ix) ?= iR 
  return (results |> iResult, addContractVersions ix iVersions vers)

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  InputVersionMap ->
  AbstractGlobalContract -> 
  String ->
  FaeTXM (InputResults, Maybe AbstractGlobalContract)
runContract iRealID vers fAbs arg = do
  thisTXID <- view _thisTXID
  ~(~(~(result, vers), gAbsM), outputsL) <- 
    listen $ callContract fAbs (arg, vers)
  let 
    iResult = gAbsM `deepseq` outputsL `seq` result
    iOutputsM = Just $ listToOutputs outputsL
    worked = unsafeIsDefined iResult
    iStatus
      | not (unsafeIsDefined iResult) = Failed
      | Nothing <- gAbsM = Deleted
      | otherwise = Updated
    iVersions
      | hasNonce iRealID = vers
      | otherwise = vers `seq` emptyVersionMap
  -- The actual result of the contract includes both 'result' and also
  -- the outputs and continuation function, so we link their fates.
  let 
  iExportedResult <- exportReturnValue iResult
  return (InputResults{..}, gAbsM)


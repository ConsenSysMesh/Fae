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

import Control.Monad.Fix
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
type TXStorageM = FaeStorageT FaeTXM

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
runTransaction f fallback inputArgs txID txSigners isReward = runTX $ do
  inputResults <- runInputContracts inputArgs 
  let inputs = Vector.toList $ fmap (getWithEscrows . iResult) inputResults
  ~(result, outputs) <- lift $ runTransactionBody isReward f fallback inputs
  _getStorage . at txID ?= TransactionEntry{..}
  where
    runTX = FaeStorage . 
      (mapStateT $ Identity . fst . runFaeTXM (txData txID txSigners))

runTransactionBody :: 
  (TransactionConstraints f) =>
  Bool -> f -> [TransactionFallback f] -> 
  [ReturnValue] -> FaeTXM (Result, Outputs)
runTransactionBody isReward f fallback = withReward isReward >=> doTX f fallback

-- | Actually perform the transaction.
doTX :: 
  (TransactionConstraints f) => 
  f -> [TransactionFallback f] -> [ReturnValue] -> FaeTXM (Result, Outputs)
doTX f fallbacks x = 
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
runInputContracts :: Inputs -> TXStorageM (Vector InputResults)
runInputContracts inputs = mfix $ 
  forM (Vector.fromList inputs) . nextInput . makeVers 
  where makeVers = InputVersionMap . fmap iVersions

-- | Looks up the contract ID and dispatches to the various possible
-- actions (call, import, error) depending on what's found.
nextInput ::
  InputVersionMap -> (ContractID, String, Renames) -> TXStorageM InputResults
nextInput vers (cID, arg, renames) = do
  valEM <- use $ at cID
  maybe errInputResult (either importedCallResult cCall) valEM cID
  where cCall = contractCallResult vers arg renames

-- | If the contract function is available, use it, update if it suceeds,
-- and produce the results regardless.
contractCallResult :: 
  InputVersionMap -> String -> Renames -> 
  OutputData -> ContractID -> TXStorageM InputResults
contractCallResult vers arg (Renames rMap) OutputData{..} cID = do
  ~(iR, gAbsM) <- lift $ 
    pushArg arg . remapSigners rMap $ 
      runContract cID vers outputContract arg
  when (successful iR) $ contractAtCID cID .= gAbsM

  -- The nonce needs to be made explicit for the purpose of exporting
  -- the return value with correct information of when it applies.
  return $ iR & _iRealID . _contractNonce .~ Nonce outputNonce

-- | If instead we have an imported value, just use that.  
importedCallResult :: InputResults -> ContractID -> TXStorageM InputResults
importedCallResult iR@InputResults{..} cID = do
  at cID .= Nothing -- Just to make sure
  _ <- lift $ putEscrows iResult
  return iR

-- | Otherwise, this input is exceptional (and so, probably, is the
-- transaction result).
errInputResult :: ContractID -> TXStorageM InputResults
-- A quick way of assigning the same exception to all the fields of the
-- 'InputResults', but setting the 'iRealID' and 'iStatus' to actual
-- values, which are useful even if the contract itself is missing.
errInputResult cID = return $ (throw $ BadContractID cID) & 
  \ ~InputResults{..} -> InputResults{iRealID = cID, iStatus = Failed, ..}

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
  ~(~(~(resultE, vers), gAbsM), outputsL) <- 
    listen $ callContract fAbs (arg, vers)
  let 
    iResult = gAbsM `deepseq` outputsL `deepseq` resultE
    iOutputsM = Just $ listToOutputs outputsL
    iStatus
      | not (unsafeIsDefined iResult) = Failed
      | Nothing <- gAbsM = Deleted
      | otherwise = Updated
    iVersions
      | hasNonce iRealID = vers
      | otherwise = vers `seq` emptyVersionMap
  return (InputResults{..}, gAbsM)


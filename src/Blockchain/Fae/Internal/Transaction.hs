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

import GHC.Generics

-- * Types

-- | How inputs are provided to transactions.  The contract with a given ID
-- gets an argument and a (possibly empty) remapping of signer names.
data Input =
  InputArgs
  {
    inputCID :: ContractID,
    inputArg :: String,
    inputRenames :: Renames
  } |
  InputReward
  deriving (Generic)

-- | Transaction monad with storage access, for executing contracts and
-- saving the results.
type TXStorageM = FaeStorageT FaeTXM

-- * Typeclasses

type family FallbackType a where
  FallbackType (a -> f) = a -> FallbackType f
  FallbackType (FaeTX a) = FaeTX ()

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
class TransactionBody f where
  -- | The workhorse of this module; steps through the arguments,
  -- accumulating the input results and the final return value.
  --
  -- The 'Result', in our instances, is added in this function, and is
  -- guaranteed to be defined even if the return value is exceptional,
  -- unless the transaction itself is exceptional, when it throws that
  -- exception.
  callTransactionBody :: 
    f -> [Input] -> TXStorageM ([InputResults], Result, [Output])

  -- | "Installs a handler" on the given transaction body.
  applyFallback :: FallbackType f -> f -> f

{- Instances -}

instance Serialize Input
instance NFData Input

-- | The base-case instance: a transaction with no arguments.
instance (Show a) => TransactionBody (FaeTX a) where
  callTransactionBody doBody [] = do
    (result, outputs) <- lift $ listen $ Result <$> getFaeTX doBody
    return ([], result, outputs)
  callTransactionBody _ inputs = return (errIRs, err, err) where 
    err = throw BadSignature
    errIRs = fst . errInputResult (const UnexpectedInput) . inputCID <$> inputs

  applyFallback doFallback doBody = FaeTX $ do
    escrows <- get
    y <- getFaeTX doBody
    if unsafeIsDefined y
    then return y
    else put escrows >> getFaeTX doFallback >> return y

-- | The inductive-case instance: a transaction with one more argument.
instance (TransactionBody f, Typeable a) => TransactionBody (a -> f) where
  callTransactionBody g [] = return ([], err, err) where 
    err = throw NotEnoughInputs
  callTransactionBody g (InputReward : rest) = 
    (_1 .~ err) . (_2 .~ err) <$> callTransactionBody (g err) rest 
    where err = throw UnexpectedReward
  callTransactionBody g (InputArgs{..} : rest) = do
    valEM <- use $ at inputCID 
    (iR, newStoredContractM) <- lift $ 
      doInput (inputCID, inputArg, inputRenames) valEM
    let rv = getWithEscrows $ iResult iR
        e = BadArgType (returnValueType rv) (typeRep $ Proxy @a)
        errIR = errInputResult (const e) (iRealID iR) ^. _1
        (x, inputResults) = maybe (throw e, errIR) (,iR) $ getReturnValue rv
    (restResults, txResult, outputs) <- callTransactionBody (g x) rest

    -- Previously also updated the contract version in 'iRealID', but that
    -- muddles the summary output and also allows exporting things that were
    -- not called by version, which is actually not a good idea since the
    -- version should be explicit in the transaction message for
    -- verification.
    when (successful inputResults) $ at inputCID .= fmap Right newStoredContractM

    -- This /should/ undefine @txResult@ only if the contract call has the
    -- wrong type, if the contract ID was bad, or if the contract version
    -- was bad.  Any other kind of error should be contained inside the
    -- 'ReturnValue' of 'iResult'.
    let checkRV :: b -> b
        checkRV = seq (iResult inputResults)
    return (inputResults : restResults, checkRV txResult, checkRV outputs)

  applyFallback doFallback doBody x = applyFallback (doFallback x) (doBody x)

instance {-# OVERLAPPING #-} 
  (TransactionBody f) => TransactionBody (Reward -> f) where

  callTransactionBody g (InputReward : rest) = do
    eID <- newEscrow RewardName
    callTransactionBody (g $ Reward eID) rest
  callTransactionBody g inputs = 
    (_1 .~ err) . (_2 .~ err) <$> callTransactionBody (g err) inputs
    where err = throw ExpectedReward

  applyFallback doFallback doBody x = applyFallback (doFallback x) (doBody x)

-- * Functions

-- ** Running transactions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.
runTransaction :: 
  (TransactionBody f) =>
  f -> [FallbackType f] -> [Input] -> TransactionID -> Signers -> Bool -> 
  FaeStorage ()
runTransaction f0 fallbacks inputs0 txID txSigners isReward = runTX $ do
  let f = foldr applyFallback f0 fallbacks
      inputs
        | isReward = InputReward : inputs0
        | otherwise = inputs0
  ~(iRs, result, os) <- callTransactionBody f inputs
  let inputResults = Vector.fromList iRs
      outputs = Vector.fromList os
  _getStorage . at txID ?= TransactionEntry{..}
  where
    runTX = FaeStorage . 
      (mapStateT $ Identity . fst . runFaeTXM (txData txID txSigners))

-- ** Running contracts

-- | Looks up the contract ID and dispatches to the various possible
-- actions (call, import, error) depending on what's found.
--
-- Guarantee: the 'InputResults' is never undefined, even if an error
-- occurred during execution of the input call.
doInput :: 
  (ContractID, String, Renames) -> Maybe (IxValue Storage) ->
  FaeTXM (InputResults, Maybe StoredContract)
-- None of the alternatives here should throw an exception.  If an
-- exception is to be returned, it needs to be via `errInputResult`, so
-- that at least the input /has/ a result.
doInput (cID, arg, renames) valEM = cID & 
  maybe (return . errInputResult BadContractID) 
        (either importedCallResult cCall) 
        valEM 
  where cCall = contractCallResult arg renames

-- | Uses the stored contract that was found, returning a full
-- 'InputResults' (including possible exceptional return values) and the
-- possible updated contract.
contractCallResult :: 
  String -> Renames -> StoredContract -> ContractID -> 
  FaeTXM (InputResults, Maybe StoredContract)
contractCallResult arg (Renames rMap) StoredContract{..} cID 
  | contractVersion cID `matchesVersion` Version storedVersion = do
      runResult <- pushArg arg . remapSigners rMap $ 
        runContract cID storedVersion storedFunction arg
      return $ runResult & _2 %~ fmap (flip StoredContract nextVersion)
  | otherwise = return $ errInputResult (BadContractVersion storedVersion) cID
  where nextVersion = digest (storedVersion, arg)

-- | If there's no stored contract but there is an imported result for this
-- call, we use that, simulating its execution by depositing its escrows
-- into context.  This will never fail, and will also never update the
-- contract (which is not present).
importedCallResult :: 
  InputResults -> ContractID -> FaeTXM (InputResults, Maybe StoredContract)
importedCallResult iR@InputResults{..} cID = do
  _ <- putEscrows iResult
  return (iR, Nothing)

-- | Constructs a conforming exceptional input result, containing all the
-- necessary fields for the summary with exceptions in the ones that can't
-- be known.
errInputResult :: 
  (Exception e) => 
  (ContractID -> e) -> ContractID -> (InputResults, Maybe StoredContract)
-- A quick way of assigning the same exception to all the fields of the
-- 'InputResults', but setting the 'iRealID' and 'iStatus' to actual
-- values, which are useful even if the contract itself is missing.
errInputResult ef cID = throw (ef cID) & (,Nothing) .
  \ ~InputResults{..} -> InputResults{iRealID = cID, iStatus = Failed, ..}

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  VersionID ->
  AbstractGlobalContract -> 
  String ->
  FaeTXM (InputResults, Maybe AbstractGlobalContract)
runContract iRealID iVersionID fAbs arg = do
  ~(~(iResult, gAbsM), outputsL) <- listen $ callContract fAbs arg
  let iOutputsM = Just $ Vector.fromList outputsL
      iStatus
        | ReturnValue rv <- getWithEscrows iResult,
          not (unsafeIsDefined rv) = Failed
        | Nothing <- gAbsM = Deleted
        | otherwise = Updated
  return (InputResults{..}, gAbsM)


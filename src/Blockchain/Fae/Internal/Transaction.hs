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
    inputRenames :: Renames,
    inputInputs :: InputMaterials
  } |
  InputReward
  deriving (Generic)

-- | Supplementary contract calls provided separately and not passed
-- directly as arguments but rather implicitly as part of the transaction
-- state.  This shows up as a 'Map' but is provided as a list because we
-- want to ensure execution ordering in case the same contract is called
-- twice (the only way calls' state changes can interact).
type InputMaterials = [(String, Input)]

-- | Transaction monad with storage access, for executing contracts and
-- saving the results.  Note that outputs and escrows have been hidden;
-- this is so contracts and the transaction body, and the fallbacks, have
-- completely independent output lists.
type TXStorageM = FaeStorageT TXBodyM

-- | The monad in which the transaction body is executed.  Escrows are
-- accumulated from the various contract calls, but no outputs are
-- accumulated, because those are collected individually from each call and
-- from the body itself.  This elision makes it impossible to express
-- various subtle error states involving undefined outputs.
type TXBodyM = EscrowsT TXDataM

-- * Typeclasses

-- | Replaces the ultimate return type with a partially-run monad, so as to
-- stabilize the outputs.
type family GlobalType a = t | t -> a where
  GlobalType (a -> f) = a -> GlobalType f
  GlobalType (FaeTX a) = TXBodyM (a, [Output])

class Globalizable f where
  globalize :: f -> GlobalType f

-- | Applies to any function of any arity, ending in a 'FaeTX', which it
-- voids.
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
class (Globalizable f, Globalizable (FallbackType f)) => TransactionBody f where
  -- | The workhorse of this module; steps through the arguments,
  -- accumulating the input results and the final return value.
  --
  -- The 'Result', in our instances, is added in this function, and is
  -- guaranteed to be defined even if the return value is exceptional,
  -- unless the transaction itself is exceptional, when it throws that
  -- exception.
  callTransactionBody :: 
    GlobalType f -> [Input] -> TXStorageM ([InputResults], Result, [Output])

  -- | "Installs a handler" on the given transaction body.
  applyFallback :: GlobalType f -> GlobalType (FallbackType f) -> GlobalType f

{- Instances -}

instance Serialize Input
instance NFData Input

-- | The base-case instance: a transaction with no arguments.
instance (Show a) => TransactionBody (FaeTX a) where
  callTransactionBody doBody [] = lift $ do
    ~(result, os) <- doBody
    let outputs = if unsafeIsDefined os then os else []
    return ([], Result result, outputs)
  callTransactionBody doBody (InputReward : rest) =
    (_2 .~ err) . (_3 .~ []) <$> callTransactionBody doBody rest 
    where err = throw UnexpectedReward
  callTransactionBody _ inputs = return (errIRs, err, []) where 
    err = throw BadSignature
    errIRs = fst . errInputResult (const UnexpectedInput) . inputCID <$> inputs

  applyFallback body fallback = do
    escrows <- get
    ~(y, os) <- body
    let z = os `deepseq` y
    if unsafeIsDefined z
    then return (z, os)
    else (_1 .~ z) <$> (put escrows >> fallback)

instance Globalizable (FaeTX a) where
  globalize = mapStateT (fmap sw . runWriterT) . getFaeTX where 
    sw ~(~(x, s), w) = ((x, w), s)

-- | The inductive-case instance: a transaction with one more argument.
instance (TransactionBody f, Typeable a) => TransactionBody (a -> f) where
  callTransactionBody _ [] = return ([], err, []) where 
    err = throw NotEnoughInputs
  callTransactionBody g (InputReward : rest) = 
    (_2 .~ err) . (_3 .~ []) <$> callTransactionBody (g err) rest 
    where err = throw UnexpectedReward
  callTransactionBody g (inputArgs : rest) = do
    (x, inputResults) <- runInputArgs getReturnValue inputArgs 
    (restResults, result, outputs) <- callTransactionBody (g x) rest
    return (inputResults : restResults, result, outputs)

  applyFallback body fallback x = applyFallback (body x) (fallback x)

instance (Globalizable f) => Globalizable (a -> f) where
  globalize = fmap globalize

-- | The special-case instance: a reward.
instance {-# OVERLAPPING #-} 
  (TransactionBody f) => TransactionBody (Reward -> f) where

  callTransactionBody g (InputReward : rest) = do
    x <- lift $ Reward . fst <$> globalize (newEscrow @FaeTX RewardName)
    callTransactionBody (g x) rest
  callTransactionBody g inputs = 
    (_2 .~ err) . (_3 .~ []) <$> callTransactionBody (g err) inputs
    where err = throw ExpectedReward

  applyFallback body fallback x = applyFallback (body x) (fallback x)

instance {-# OVERLAPPING #-} (Globalizable f) => Globalizable (Reward -> f) where
  globalize = fmap globalize

-- * Functions

-- ** Running transactions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.
runTransaction :: 
  (TransactionBody f) =>
  f -> [FallbackType f] -> 
  InputMaterials -> [Input] -> 
  TransactionID -> Signers -> Bool -> 
  FaeStorage ()
runTransaction f fbs materialArgs inputs0 txID txSigners isReward = runTX $ do
  let body = foldl applyFallback (globalize f) (globalize <$> fbs)
      inputs
        | isReward = InputReward : inputs0
        | otherwise = inputs0
  (localMaterials, inputMaterials) <- runMaterials materialArgs
  -- 'callTransactionBody' really must not throw an exception, so we don't
  -- lazy pattern-match because that would just make things worse.
  (iRs, result, os) <- 
    local (_localMaterials .~ localMaterials) $ callTransactionBody body inputs
  let inputResults = Vector.fromList iRs
      outputs = Vector.fromList os
  _getStorage . at txID ?= TransactionEntry{..}
  where runTX = FaeStorage . mapStateT (flip runReaderT r0 . flip evalStateT s0)
        r0 = txData txID txSigners
        s0 = Escrows Map.empty txID

-- | Runs the materials as inputs, providing both kinds of result and /not/
-- type-checking the return values (that is done at the point of use).
runMaterials :: InputMaterials -> TXStorageM (MaterialsMap, Materials)
runMaterials = 
  fmap ((_1 %~ Map.fromList) . (_2 %~ Vector.fromList) . unzip . map split) .
  traverse (traverse $ runInputArgs Just) 
  where split (name, (x, inputResults)) = ((name, x), (name, inputResults))

-- | This is apparently polymorphic, but in fact will fail if the return
-- type does not agree with the (dynamic) input type.  It is also actually
-- (not apparently) partial, as it does not handle 'InputReward', whose
-- result is not dynamically typed.  Other than the (completely avoidable)
-- exception thrown by calling the missing case, this function never
-- throws.
--
-- Renames are applied here so that they propagate to the materials calls.
-- This ensures locality in the renaming logic as it is applied
-- recursively.
runInputArgs :: 
  forall a. 
  (Typeable a) => (ReturnValue -> Maybe a) -> Input -> TXStorageM (a, InputResults)
runInputArgs f InputArgs{inputRenames = Renames rMap, ..} = remapSigners rMap $ do
  (localMaterials, iMaterialsM) <- (_2 %~ Just) <$> runMaterials inputInputs
  valEM <- use $ at inputCID 
  (iR, newStoredContractM) <- lift . lift $ 
    -- The transaction's local materials do not propagate to its regular
    -- inputs.  Note that none of this contract's local materials
    -- propagate to their own materials calls either; each contract gets
    -- just the materials that are declared exactly for it.
    local (_localMaterials .~ localMaterials) $
    doInput inputCID inputArg valEM
  rv <- lift $ putEscrows $ iResult iR
  let cID = iRealID iR
      e = BadArgType (returnValueType rv) (typeRep $ Proxy @a)
      errIR = errInputResult (const e) cID ^. _1
      (x, inputResults)
        | unsafeIsDefined rv =
            maybe (throw BadSignature, errIR) (,iR) $ f rv
        | otherwise = (throw $ InputFailed cID, iR)
  -- Previously also updated the contract version in 'iRealID', but that
  -- muddles the summary output and also allows exporting things that were
  -- not called by version, which is actually not a good idea since the
  -- version should be explicit in the transaction message for
  -- verification.
  when (successful inputResults) $ at inputCID .= (Right <$> newStoredContractM)
  return (x, inputResults{iMaterialsM})

-- ** Running contracts

-- | Looks up the contract ID and dispatches to the various possible
-- actions (call, import, error) depending on what's found.
--
-- Guarantee: the 'InputResults' is never undefined, even if an error
-- occurred during execution of the input call.
doInput :: 
  ContractID -> String -> Maybe (IxValue Storage) ->
  TXDataM (InputResults, Maybe StoredContract)
-- None of the alternatives here should throw an exception.  If an
-- exception is to be returned, it needs to be via `errInputResult`, so
-- that at least the input /has/ a result.
doInput cID arg valEM = cID & 
  maybe (return . errInputResult BadContractID) 
        (either importedCallResult $ contractCallResult arg) 
        valEM 

-- | Uses the stored contract that was found, returning a full
-- 'InputResults' (including possible exceptional return values) and the
-- possible updated contract.
--
-- The local hash is modified by hashing in the argument here, rather than
-- in 'runInputArgs', because each of the materials calls gets its own
-- local hash from its own argument.
contractCallResult :: 
  String -> StoredContract -> ContractID -> 
  TXDataM (InputResults, Maybe StoredContract)
contractCallResult arg StoredContract{..} cID 
  | contractVersion cID `matchesVersion` Version storedVersion = do
      runResult <- pushArg arg $ 
        runContract cID storedVersion storedFunction arg
      return $ runResult & _2 %~ fmap (flip StoredContract nextVersion)
  | otherwise = return $ errInputResult (BadContractVersion storedVersion) cID
  where nextVersion = digest (storedVersion, arg)

-- | If there's no stored contract but there is an imported result for this
-- call, we use that, simulating its execution by depositing its escrows
-- into context.  This will never fail, and will also never update the
-- contract (which is not present).
importedCallResult :: 
  InputResults -> ContractID -> TXDataM (InputResults, Maybe StoredContract)
importedCallResult = const . return . (,Nothing)

-- | Constructs a conforming exceptional input result, containing all the
-- necessary fields for the summary with exceptions in the ones that can't
-- be known.
errInputResult :: 
  (Exception e) => 
  (ContractID -> e) -> ContractID -> (InputResults, Maybe StoredContract)
-- A quick way of assigning the same exception to all the fields of the
-- 'InputResults', but setting the 'iRealID' and 'iStatus' to actual
-- values, which are useful even if the contract itself is missing.
errInputResult ef cID = (,Nothing) $ throw (ef cID) & 
  \ ~InputResults{iResult = ~(WithEscrows es rv), ..} -> 
    InputResults{iRealID = cID, iStatus = Failed, iResult = WithEscrows es rv, ..}

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  VersionID ->
  AbstractGlobalContract -> 
  String ->
  TXDataM (InputResults, Maybe AbstractGlobalContract)
runContract iRealID iVersionID fAbs arg = do
  ~(~(returnValue, escrowMap, outputsL), gAbsM) <- callContract fAbs arg
  let enforce :: a -> a
      enforce = seq outputsL . seq escrowMap . seq returnValue
      iOutputsM = Just $ Vector.fromList outputsL
      iResult = WithEscrows (enforce escrowMap) (enforce returnValue)
      iStatus
        | not (unsafeIsDefined returnValue) = Failed
        | Nothing <- gAbsM = Deleted
        | otherwise = Updated
      iMaterialsM = Nothing -- Changed in 'runInputArgs'
  return (InputResults{..}, gAbsM)


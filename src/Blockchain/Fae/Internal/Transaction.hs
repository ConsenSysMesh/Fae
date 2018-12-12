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

-- | Only to cause the transaction signature check to fail.
data BadType

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

-- * Typeclasses

-- | Replaces the ultimate return type with a partially-run monad, so as to
-- stabilize the outputs.
type family GlobalType a = t | t -> a where
  GlobalType (a -> f) = a -> GlobalType f
  GlobalType (FaeTX a) = EscrowsT TXDataM (a, [Output])

-- | Helper class to define an overloaded function that reduces the monad
-- that a transaction body returns into.
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

-- | -
instance Serialize Input
-- | -
instance NFData Input

-- | The base-case instance: a transaction with no arguments.
instance (Show a) => TransactionBody (FaeTX a) where
  callTransactionBody doBody [] = lift $ do
    txID <- view _thisTXID
    escrows <- pop
    ~(result, os) <- lift $ evalStateT doBody $ Escrows escrows txID
    let outputs = if unsafeIsDefined os then os else []
    return ([], Result result, outputs)
  callTransactionBody doBody (InputReward : rest) =
    (_2 .~ err) . (_3 .~ []) <$> callTransactionBody doBody rest 
    where err = throw UnexpectedReward
  callTransactionBody _ inputs = do
    iRs <- foldInputErrors $ \case
      InputReward -> return Nothing
      input -> do
        iR <- snd <$> runBadInput (const Nothing) input
        let errIR = fst $ errInputResult (const UnexpectedInput) (inputCID input)
        return $ Just errIR{iMaterialsM = iMaterialsM iR}
    return (iRs, throw BadSignature, []) 
    where 
      runBadInput = runInputArgs @BadType (const $ typeRep $ Proxy @BadType)
      foldInputErrors f = 
        foldl (\mIRs i -> maybe id (:) <$> f i <*> mIRs) (return []) inputs

  applyFallback body fallback = do
    escrows <- get
    ~(y, os) <- body
    let z = os `deepseq` y
    if unsafeIsDefined z
    then return (z, os)
    else (_1 .~ z) <$> (put escrows >> fallback)

-- | -
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
    (x, inputResults) <- runInputArgs contractValType getReturnValue inputArgs 
    (restResults, result, outputs) <- callTransactionBody (g x) rest
    return (inputResults : restResults, result, outputs)

  applyFallback body fallback x = applyFallback (body x) (fallback x)

-- | -
instance (Globalizable f) => Globalizable (a -> f) where
  globalize = fmap globalize

-- | The special-case instance: a reward.
instance {-# OVERLAPPING #-} 
  (TransactionBody f) => TransactionBody (Reward -> f) where

  callTransactionBody g (InputReward : rest) = do
    x <- lift . keepEscrows $ 
      Reward . fst <$> globalize (newEscrow @_ @FaeTX RewardName)
    callTransactionBody (g x) rest
    where
      keepEscrows :: EscrowsT TXDataM a -> TXBodyM a
      keepEscrows xm = do
        txID <- view _thisTXID
        (x, Escrows{..}) <- lift $ runStateT xm $ Escrows Map.empty txID
        keep escrowMap
        return x

  callTransactionBody g inputs = 
    (_2 .~ err) . (_3 .~ []) <$> callTransactionBody (g err) inputs
    where err = throw ExpectedReward

  applyFallback body fallback x = applyFallback (body x) (fallback x)

-- | -
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
  where runTX = FaeStorage . mapStateT (flip runReaderT r0 . flip evalStateT [])
        r0 = txData txID txSigners

-- | Runs the materials as inputs, providing both kinds of result and /not/
-- type-checking the return values (that is done at the point of use).
runMaterials :: InputMaterials -> TXStorageM (MaterialsMap, Materials)
runMaterials ims = do
  lift $ push Map.empty
  fmap ((_1 %~ Map.fromList) . (_2 %~ Vector.fromList) . unzip . map split) $
    traverse (traverse runMaterialInput) uniqueIMs
  where 
    runMaterialInput = 
      runInputArgs @ReturnValue (const $ typeRep $ Proxy @ReturnValue) Just
    split (name, (x, inputResults)) = ((name, x), (name, inputResults))
    uniqueIMs = adjust <$> ims
    adjust (name, input) = (name,) $
      if name `Map.member` repSet
      -- A materials call /cannot/ be anything other than an 'InputArgs'.
      then input{inputArg = throw $ RepeatedMaterial name}
      else input
    repSet = Map.filter (> 1) $ Map.fromListWith (+) $ (_2 .~ 1) <$> ims

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
  (Typeable a) => 
  (Output -> TypeRep) -> 
  (ReturnValue -> Maybe a) -> 
  Input -> 
  TXStorageM (a, InputResults)
runInputArgs getOutputType getRV args = remapSigners rMap $ do
  (localMaterials, iMaterialsM) <- (_2 %~ Just) <$> runMaterials inputInputs
  (getResult, ty) <- gets $ atCID inputCID getImport getContract errIR
  (x, (iR, newStoredContractF)) <- 
    if ty == thisTy
    then do
      -- The transaction's local materials do not propagate to its regular
      -- inputs.  Note that none of this contract's local materials
      -- propagate to their own materials calls either; each contract gets
      -- just the materials that are declared exactly for it.
      result <- lift $ local (_localMaterials .~ localMaterials) getResult
      let ~(WithEscrows escrows rv) = iResult $ result ^. _1
          -- This is terrible, but the type check in the enclosing conditional
          -- should ensure that this really always is @Just@.  We do in one
          -- place pass @const Nothing@, but also a bad type, specifically to
          -- trigger the type error, so this line isn't reached.
          x = fromJust $ getRV rv
      lift $ keep escrows
      return (x, result)
    else return (throw sigErr, errInputResult (const $ typeErr ty) inputCID)
  -- Previously also updated the contract version in 'iRealID', but that
  -- muddles the summary output and also allows exporting things that were
  -- not called by version, which is actually not a good idea since the
  -- version should be explicit in the transaction message for
  -- verification.
  outputAt inputCID %= fmap (_storedContract %~ (>>= newStoredContractF))
  return (x, iR{iMaterialsM})

  where
    InputArgs{inputRenames = Renames rMap, ..} = args
    getImport iR cID = 
      (importedCallResult iR cID, returnValueType . getWithEscrows . iResult $ iR)
    getContract o cID = 
      (contractCallResult inputArg (storedContract o) cID, getOutputType o)
    thisTy = typeRep $ Proxy @a
    errIR = (, thisTy) . return . errInputResult BadContractID 
    sigErr = BadSignature
    typeErr = flip BadArgType thisTy

-- ** Running contracts

-- | Uses the stored contract that was found, returning a full
-- 'InputResults' (including possible exceptional return values) and the
-- possible updated contract.
--
-- The local hash is modified by hashing in the argument here, rather than
-- in 'runInputArgs', because each of the materials calls gets its own
-- local hash from its own argument.
contractCallResult :: 
  String -> Maybe StoredContract -> ContractID -> 
  TXBodyM (InputResults, StoredContract -> Maybe StoredContract)
contractCallResult arg scM cID = do
  (iR, newFM) <- runContract cID storedVersion storedFunction arg
  -- The new version reflects the entire transaction that calls the
  -- contract, and therefore, everything that might change its state.
  nextVersion <- asks $ digest . (storedVersion,) . thisTXID
  let newSCF = (mapMOf _storedFunction newFM) . (_storedVersion .~ nextVersion)
  return (iR, newSCF)
  where 
    deletedErr = throw (ContractDeleted cID)
    badVersionErr = throw (BadContractVersion storedVersion cID)
    ~StoredContract{..} 
      | ~sc@StoredContract{storedVersion = sv} <- fromMaybe deletedErr scM,
        contractVersion cID `matchesVersion` Version sv = sc
      | otherwise = badVersionErr

-- | If there's no stored contract but there is an imported result for this
-- call, we use that, simulating its execution by depositing its escrows
-- into context.  This will never fail, and will also never update the
-- contract (which is not present).
importedCallResult :: 
  InputResults -> ContractID -> 
  TXBodyM (InputResults, StoredContract -> Maybe StoredContract)
importedCallResult = const . return . (,Just)

-- | Constructs a conforming exceptional input result, containing all the
-- necessary fields for the summary with exceptions in the ones that can't
-- be known.
errInputResult :: 
  (Exception e) => 
  (ContractID -> e) -> ContractID -> 
  (InputResults, StoredContract -> Maybe StoredContract)
errInputResult ef cID = (errIR, Just) where
  errIR = (cloakInputResults cID $ throw $ ef cID){iStatus = Failed}

-- | Executes the contract function, returning the result and the
-- continuation function.  This is where the 'iStatus' field of the
-- 'InputResults' value is set, because this is the last place where the
-- 'Maybe' on the continuation is readily available.
runContract ::
  ContractID ->
  VersionID ->
  AbstractGlobalContract -> 
  String ->
  TXBodyM (InputResults, AbstractGlobalContract -> Maybe AbstractGlobalContract)
runContract iRealID iVersionID fAbs arg = do
  ~(~(result, outputsL), gAbsM) <- callContract fAbs arg
  let iOutputsM = Just $ Vector.fromList outputsL
      ~(iResult, iStatus, newFM)
        | not (unsafeIsDefined gAbsM) = (WithEscrows failed failed, Failed, Just)
        | Nothing <- gAbsM = (result, Deleted, const Nothing)
        | otherwise = (result, Updated, const gAbsM)
  return (InputResults{..}, newFM)

  where
    iMaterialsM = Nothing -- Changed in 'runInputArgs'

    failed :: a
    failed = throw $ InputFailed iRealID

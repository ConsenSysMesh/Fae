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
import Data.Semigroup ((<>))
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
runInputContracts = fmap fst .
  foldl' (>>=) (return ([], emptyVersionMap')) .
  zipWith nextInput [0 ..] 

-- | If the contract function is available, run it as a monadic state
-- update, adding the result to the ongoing list and updating the map of
-- all defined versions.  If instead we have an imported value, just use
-- that.  Otherwise, this input is exceptional (and so, probably, is the
-- transaction result).
nextInput ::
  Int -> (ContractID, String, Renames) -> 
  ([ReturnValue], VersionMap') -> TXStorageM ([ReturnValue], VersionMap')
nextInput ix (cID, arg, Renames renames) (results, vers) = do
  valE <- use $ at cID . defaultLens (throw $ BadContractID cID)

  (iR, vers') <- case valE of
    Right fAbs -> do 
      ~(iR, vers', gAbsM) <- lift $ 
        local (_localHash .~ digest arg) . remapSigners renames $ 
          runContract cID vers fAbs arg

      -- We don't update the contract if it didn't return cleanly
      let successful = unsafeIsDefined iR
      when successful $ at cID .= (Right <$> gAbsM)

      return (iR, vers')

    Left (x, vMap) -> do
      iResult <- lift $ putEscrows x
      at cID .= Nothing -- Just to make sure
      let (iVersions, vers') = makeOV cID vMap vers
          iRealID = cID
          iOutputsM = Nothing
      iExportedResult <- lift $ exportReturnValue iResult
      return (InputResults{..}, vers')

  txID <- view _thisTXID
  _getStorage . at txID . txInputLens ix ?= iR 
  return (results |> iResult iR, vers')

-- | Executes the contract function, returning the result, the structured
-- outputs, the new map of all currently defined versions, and the
-- continuation function.
runContract ::
  ContractID ->
  VersionMap' ->
  AbstractGlobalContract -> 
  String ->
  FaeTXM (InputResults, VersionMap', Maybe AbstractGlobalContract)
runContract cID vers fAbs arg = do
  thisTXID <- view _thisTXID
  ~(~(~(result, vMap), gAbsM), outputsL) <- listen $ callContract fAbs (arg, vers)
  let 
    -- We store this with the nonce (if given) so that if (and when) it is
    -- exported, the correct id-with-nonce can be retrieved.  Once the
    -- contract is called again, its current nonce is no longer the correct
    -- one for this result.
    iRealID = cID
    iOutputsM = Just $ listToOutputs outputsL
    ~(iVersions, vers') = makeOV cID vMap vers
  -- The actual result of the contract includes both 'result' and also
  -- the outputs and continuation function, so we link their fates.
  let iResult = gAbsM `deepseq` outputsL `seq` result
  iExportedResult <- exportReturnValue iResult
  return (InputResults{..}, vers', gAbsM)

-- | Adds the new version map to the ongoing total, but only if the
-- contract ID has a nonce.  This restriction is part of the guarantee that
-- using a versioned value produces exactly the expected result.
makeOV :: ContractID -> VersionMap -> VersionMap' -> (VersionRepMap, VersionMap')
makeOV cID vMap vers
  | hasNonce cID = 
      (
        VersionMap $ bearerType <$> getVersionMap vMap, 
        addContractVersions cID vMap vers
      )
  | otherwise = (emptyVersionMap, vers)


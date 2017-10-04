module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Coroutine
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.MonadFae
import Blockchain.Fae.Internal.Reward
import Blockchain.Fae.Internal.Storage

import Control.Monad
import Control.Monad.RWS
import Control.Monad.State

import Data.Dynamic
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq)
import Data.Typeable
import Data.Void

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GHC.Generics hiding (to)
import qualified GHC.Generics as Gen (to)

import Debug.Trace

{- Types -}

type Storage = StorageT AbstractContract
type InputOutputs = InputOutputsT AbstractContract
type FaeStorage = FaeStorageT AbstractContract

type Transaction a b = a -> FaeTX b

{- Typeclasses -}

class GetInputValues a where
  getInputValues :: [Dynamic] -> a
  default getInputValues :: 
    (Generic a, GGetInputValues (Rep a)) => [Dynamic] -> a
  getInputValues s
    | null s' = Gen.to x
    | otherwise = throw TooManyInputs
    where (x, s') = runState gGetInputValues s

class GGetInputValues f where
  gGetInputValues :: State [Dynamic] (f p)

{- Instances -}

instance GetInputValues Void where
  getInputValues [] = undefined
  getInputValues _ = throw TooManyInputs

instance (Typeable a, Typeable b) => GetInputValues (a, b)

instance {-# OVERLAPPABLE #-} (Typeable a) => GetInputValues a where
  getInputValues [xDyn] = fromDyn xDyn $
    throw $ BadArgType (typeRep (Proxy @a)) (dynTypeRep xDyn)
  getInputValues [] = throw NotEnoughInputs
  getInputValues _ = throw TooManyInputs

instance GGetInputValues U1 where
  gGetInputValues = return U1

instance (GGetInputValues f, GGetInputValues g) => GGetInputValues (f :*: g) where
  gGetInputValues = do
    l <- gGetInputValues
    r <- gGetInputValues
    return $ l :*: r

instance (Typeable c) => GGetInputValues (K1 i c) where
  gGetInputValues = do
    s <- get
    case s of
      [] -> throw NotEnoughInputs
      xDyn : s' -> do
        put s'
        return $ K1 $ fromDyn xDyn $
          throw $ BadArgType (typeRep (Proxy @c)) (dynTypeRep xDyn)

instance (GGetInputValues f) => GGetInputValues (M1 i t f) where
  gGetInputValues = M1 <$> gGetInputValues

{- Functions -}

runTransaction :: 
  forall inputs a result.
  (GetInputValues inputs, Typeable a) =>
  TransactionID -> PublicKey -> Bool ->
  Seq (ContractID, String) -> 
  Transaction inputs a -> FaeStorage a
runTransaction txID txKey isReward inputArgs f = handleAll placeException $
  state $ 
    runFaeContract txID txKey .
    transaction txID isReward inputArgs f 
  where
    placeException e = do
      _getStorage . at txID ?=
        TransactionEntry
        {
          inputOutputs = throw e,
          outputs = throw e,
          result = throw e
        }
      return $ throw e

runFaeContract :: TransactionID -> PublicKey -> FaeContract Naught a -> a
runFaeContract txID txKey =
  fst .
  (\r s m -> evalRWS m r s) txKey (txID, 0) .
  unWrapped . 
  flip evalStateT Map.empty .
  runCoroutine

transaction :: 
  (GetInputValues inputs, Typeable a) =>
  TransactionID ->
  Bool -> 
  Seq (ContractID, String) -> 
  Transaction inputs a -> 
  Storage -> FaeContract Naught (a, Storage)
transaction txID isReward inputArgs f storage = do
  (inputs0, storage', inputOutputs) <- runInputContracts inputArgs storage
  inputs <- withReward inputs0
  (result0, outputsL) <- 
    listen $ getFae $ f $ getInputValues $ toList inputs
  let 
    result = toDyn result0
    outputs = intMapList outputsL
  escrows <- get
  unless (Map.null escrows) $ throw OpenEscrows
  let newStorage = storage' & _getStorage . at txID ?~ TransactionEntry{..}
  return (result0, newStorage)

  where
    withReward inputs
      | isReward = do
          eID <- newEscrow [] rewardEscrow
          return $ inputs |> toDyn eID
      | otherwise = return inputs

runInputContracts ::
  (Functor s) =>
  Seq (ContractID, String) ->
  Storage ->
  FaeContract s (Seq Dynamic, Storage, InputOutputs)
runInputContracts inputArgs storage = 
  (\x s f -> foldl f x s) (return (Seq.empty, storage, Map.empty)) inputArgs $
  \accM (cID, arg) -> do
    (results, storage, inputOutputs) <- accM
    let 
      results' = Seq.zip (fst <$> inputArgs) results
      ConcreteContract fAbs = 
        storage ^. at cID . defaultLens (throw $ BadInput cID)
    ((gAbsM, result), outputsL) <- listen $ fAbs arg
    censor (const []) $ return
      (
        results |> result,
        storage & at cID .~ gAbsM,
        inputOutputs & at (shorten cID) ?~ intMapList outputsL
      )


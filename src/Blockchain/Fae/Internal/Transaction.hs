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
import Data.Typeable
import Data.Void

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GHC.Generics hiding (to)
import qualified GHC.Generics as Gen (to)

{- Types -}

type Storage = StorageT AbstractContract
type InputOutputs = InputOutputsT AbstractContract
type FaeStorage = FaeStorageT AbstractContract

-- | A more semantically-meaningful type synonym.  Transactions, though
-- similar to contracts in many internal ways, differ greatly in their
-- inputs and outputs.  The argument of type 'a' is constructed from the
-- return values of the input contracts according to its 'GetInputValues'
-- instance.  The return value need not and can not contain escrows (or
-- rather, the escrow IDs it contains will not be transferred anywhere).
type Transaction a b = a -> FaeTX b

{- Typeclasses -}

-- | This class controls how a type is constructed from
-- a heterogeneously-type list of input return values.  Its default
-- instance, for any 'Generic' type, simply assigns each field of a product
-- type from successive values in the list, failing if they don't all match
-- or there are extras on one side.  Therefore it is unlikely you will have
-- to write an instance yourself; however, you do need to @instance
-- GetInputValues <your type>@ for any 'a' you choose to use.
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
  (GetInputValues inputs, HasEscrowIDs inputs, Typeable a, Show a) =>
  Digest -> TransactionID -> PublicKey -> Bool ->
  [(ContractID, String)] -> 
  Transaction inputs a -> FaeStorage ()
runTransaction dig txID txKey isReward inputArgs f = handleAll placeException $
  modify $ 
    runFaeContract dig txKey .
    transaction dig txID isReward inputArgs f 
  where
    placeException e = 
      _getStorage . at txID ?=
        TransactionEntry
        {
          inputOutputs = throw e,
          outputs = throw e,
          result = throw e :: Void
        }

runFaeContract :: Digest -> PublicKey -> FaeContract Naught a -> a
runFaeContract dig txKey =
  fst .
  (\r s m -> evalRWS m r s) txKey dig .
  unWrapped . 
  flip evalStateT Map.empty .
  runCoroutine

transaction :: 
  (GetInputValues inputs, HasEscrowIDs inputs, Typeable a, Show a) =>
  Digest ->
  TransactionID ->
  Bool -> 
  [(ContractID, String)] -> 
  Transaction inputs a -> 
  Storage -> FaeContract Naught Storage
transaction dig txID isReward inputArgs f storage = do
  (inputs0, storage', inputOutputs) <- runInputContracts dig inputArgs storage
  newEscrowSeries dig 0
  inputs <- withReward inputs0
  input <- runTXEscrows $ getInputValues $ toList inputs
  (result, outputsL) <- listen $ getFae $ f input
  let outputs = intMapList outputsL
  escrows <- get
  unless (Map.null escrows) $ throw OpenEscrows
  return $ storage' & _getStorage . at txID ?~ TransactionEntry{..}

  where
    withReward inputs
      | isReward = do
          eID <- newEscrow [] rewardEscrow
          return $ inputs |> toDyn eID
      | otherwise = return inputs

runInputContracts ::
  (Functor s) =>
  Digest ->
  [(ContractID, String)] ->
  Storage ->
  FaeContract s ([Dynamic], Storage, InputOutputs)
runInputContracts dig inputArgs storage = 
  (\x s f -> foldl f x s) 
    (return ([], storage, Map.empty)) (zip inputArgs [1 ..]) $
  \accM ((cID, arg), n) -> do
    (results, storage, inputOutputs) <- accM
    let 
      results' = zip (fst <$> inputArgs) results
      ConcreteContract fAbs = 
        storage ^. at cID . defaultLens (throw $ BadInput cID)
    newEscrowSeries dig n
    ((gAbsM, result), outputsL) <- listen $ fAbs arg
    censor (const []) $ return
      (
        results |> result,
        storage & at cID .~ gAbsM,
        inputOutputs & at (shorten cID) ?~ intMapList outputsL
      )

newEscrowSeries :: (Functor s) => Digest -> Int -> FaeContract s ()
newEscrowSeries dig n = lift $ lift $ Wrapped $ put $ digest (dig, n)

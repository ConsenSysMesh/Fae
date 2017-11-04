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
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Trans.Identity

import Data.Dynamic
import Data.Foldable
import Data.Maybe
import Data.Typeable
import Data.Void

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import GHC.Generics hiding (to)
import qualified GHC.Generics as Gen (to)

{- Types -}

type Storage = StorageT AbstractContract
type Outputs = OutputsT AbstractContract
type InputOutputs = InputOutputsT AbstractContract
type FaeStorage = FaeStorageT AbstractContract

-- | Transactions, though similar to contracts in many internal ways,
-- differ greatly in their inputs and outputs.  The argument of type 'a' is
-- constructed from the return values of the input contracts according to
-- its 'GetInputValues' instance.  The return value need not and can not
-- contain escrows (or rather, the escrow IDs it contains will not be
-- transferred anywhere).
type Transaction a b = a -> FaeTX b

-- Exception type
data TransactionException =
  NotEnoughInputs |
  TooManyInputs |
  BadInput ContractID |
  OpenEscrows 
  deriving (Typeable, Show)

{- Typeclasses -}

-- | This class controls how a type is constructed from
-- a heterogeneously-type list of input return values.  Its default
-- instance, for any 'Generic' type, simply assigns each field of a product
-- type from successive values in the list, failing if they don't all match
-- or there are extras on one side.  Therefore it is unlikely you will have
-- to write an instance yourself; however, you do need to @instance
-- GetInputValues <your type>@ for any 'a' you choose to use.
class GetInputValues a where
  getInputValues :: [Dynamic] -> (a, [Dynamic])
  default getInputValues :: 
    (Generic a, GGetInputValues (Rep a)) => [Dynamic] -> (a, [Dynamic])
  getInputValues = runState $ Gen.to <$> gGetInputValues

class GGetInputValues f where
  gGetInputValues :: State [Dynamic] (f p)

{- Instances -}

instance Exception TransactionException

instance GetInputValues Void where
  getInputValues _ = (throw TooManyInputs, [])

instance (Typeable a, Typeable b) => GetInputValues (a, b)

instance {-# OVERLAPPABLE #-} (Typeable a) => GetInputValues a where
  getInputValues (xDyn : rest) = (x, rest) where
    x = fromDyn xDyn $
      throw $ BadArgType (typeRep (Proxy @a)) (dynTypeRep xDyn)
  getInputValues [] = throw NotEnoughInputs

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
  Transaction inputs a -> [(ContractID, String)] -> 
  TransactionID -> PublicKey -> Bool -> 
  FaeStorage ()
runTransaction f inputArgs txID txKey isReward = handleAll placeException $
  modify $ 
    runFaeContract txID txKey .
    transaction txID isReward inputArgs f
  where
    placeException e = 
      _getStorage . at txID ?=
        TransactionEntry
        {
          inputOutputs = throw e,
          outputs = throw e,
          result = throw e :: Void
        }

runFaeContract :: TransactionID -> PublicKey -> FaeContract Naught a -> a
runFaeContract (ShortContractID dig) txKey =
  flip runReader txKey .
  fmap fst . runWriterT .
  flip evalStateT Escrows{escrowMap = Map.empty, nextID = dig} .
  runCoroutine

transaction :: 
  (GetInputValues input, HasEscrowIDs input, Typeable a, Show a) =>
  TransactionID ->
  Bool -> 
  [(ContractID, String)] -> 
  Transaction input a -> 
  Storage -> FaeContract Naught Storage
transaction txID isReward args f storage = do
  (input, storage', inputOutputs) <- runInputSpec args isReward storage
  (result, outputs) <- doTX f input
  return $ storage' 
    & _getStorage . at txID ?~ TransactionEntry{..}
    & _txLog %~ cons txID

doTX :: Transaction input a -> input -> FaeContract Naught (a, Outputs)
doTX f input = do
  (result, outputsL) <- listen $ getFae $ f input
  let outputs = intMapList outputsL
  escrows <- use _escrowMap
  unless (Map.null escrows) $ throw OpenEscrows
  return (result, outputs)

runInputSpec :: 
  forall input.
  (GetInputValues input, HasEscrowIDs input) =>
  [(ContractID, String)] -> Bool -> Storage -> 
  FaeContract Naught (input, Storage, InputOutputs)
runInputSpec args isReward storage = do
  triple <- runInputContracts (Proxy @input) isReward storage args
  let (input0, unused) = getInputValues $ triple ^. _1
  unless (null unused) $ throw TooManyInputs
  -- We don't have to be careful about self-referencing here because we
  -- already checked it in 'runInputContract'
  input <- runTXEscrows $ resolveEscrowLocators input0 input0 
  return (triple & _1 .~ input)
 
runInputContracts ::
  (GetInputValues input, HasEscrowIDs input) =>
  Proxy input ->
  Bool ->
  Storage ->
  [(ContractID, String)] ->
  FaeContract Naught ([Dynamic], Storage, InputOutputs)
runInputContracts p isReward storage args = do
  runIdentityT $ flip execStateT ([], storage, Map.empty) $ 
    forM_ args $ modifyM . uncurry (runInputContract p isReward)
  where modifyM f = get >>= lift . IdentityT . f >>= put

runInputContract ::
  forall input.
  (GetInputValues input, HasEscrowIDs input) =>
  Proxy input ->
  Bool ->
  ContractID ->
  String ->
  ([Dynamic], Storage, InputOutputs) ->
  FaeContract Naught ([Dynamic], Storage, InputOutputs)
runInputContract _ isReward cID argS (results, storage, inputOutputs) = do
  input <- withReward $ results ++ repeat (throw NotEnoughInputs)
  let 
    (argInput :: input, _) = getInputValues input
    arg = Input argInput argS
    ConcreteContract fAbs = 
      storage ^. at cID . defaultLens (throw $ BadInput cID)
  ((gAbsM, result), outputsL) <- listen $ fAbs arg
  censor (const []) $ return $
    (
      results |> result,
      storage & at cID .~ gAbsM,
      inputOutputs & at (shorten cID) ?~ intMapList outputsL
    )

  where
    withReward 
      | isReward = \inputs -> do
          eID <- newEscrow [] rewardEscrow
          return $ toDyn eID : inputs
      | otherwise = return 


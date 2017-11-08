{- |
Module: Blockchain.Fae.Internal.Transaction
Description: Transaction execution
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides the code that navigates the intricacies of executing a transaction.  It is useful for front-end implementations, but not to users.
-}
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

-- * Types

-- | 'StorageT' is only parametrized because 'AbstractContract' isn't
-- defined in "Storage", which is because it would cause an import cycle.
-- Storage always contains abstract contracts.
type Storage = StorageT AbstractContract
-- | Likewise
type Outputs = OutputsT AbstractContract
-- | Likewise
type InputOutputs = InputOutputsT AbstractContract
-- | Likewise
type FaeStorage = FaeStorageT AbstractContract

-- | How inputs are provided to transactions.
type Inputs = [(ContractID, String)]

-- | Transactions, though similar to contracts in many internal ways,
-- differ greatly in their inputs and outputs.  The argument of type 'a' is
-- constructed from the return values of the input contracts according to
-- its 'GetInputValues' instance.  The return value need not and can not
-- contain escrows (or rather, the escrow IDs it contains will not be
-- transferred anywhere).
type Transaction a b = a -> FaeTX b

-- | Exception type
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

-- | Generic helper class
class GGetInputValues f where
  -- | This is the state monad, rather than having the same signature as
  -- 'getInputValues', because we need to walk through the list of dynamic
  -- inputs and progressively remove values from it.  At the same time, we
  -- need to know if there were leftovers.
  gGetInputValues :: State [Dynamic] (f p)

-- * Instances

-- | Of course
instance Exception TransactionException

-- | Empty types not only have no input values, it is insulting to suggest
-- that they might.
instance GetInputValues Void where
  getInputValues _ = (throw TooManyInputs, [])

-- | Default instance
instance (Typeable a, Typeable b) => GetInputValues (a, b)

-- | By default, any type tries to take a single input as its entire value.
-- This may not actually be the case, of course.
instance {-# OVERLAPPABLE #-} (Typeable a) => GetInputValues a where
  getInputValues (xDyn : rest) = (x, rest) where
    x = fromDyn xDyn $
      throw $ BadArgType (typeRep (Proxy @a)) (dynTypeRep xDyn)
  getInputValues [] = throw NotEnoughInputs

-- | No values in a constructor with no records
instance GGetInputValues U1 where
  gGetInputValues = return U1

-- | We only allow product types to have input values, because how would
-- one choose between different branches of a sum type?  Non-algebraic
-- types are out of luck because we don't expose the methods of
-- 'GetInputValues' to be implemented manually.
instance (GGetInputValues f, GGetInputValues g) => GGetInputValues (f :*: g) where
  gGetInputValues = do
    l <- gGetInputValues
    r <- gGetInputValues
    return $ l :*: r

-- | For a nested type, we peel off one input and fix its type.  We do
-- _not_ do a recursive call.
instance (Typeable c) => GGetInputValues (K1 i c) where
  gGetInputValues = do
    s <- get
    case s of
      [] -> throw NotEnoughInputs
      xDyn : s' -> do
        put s'
        return $ K1 $ fromDyn xDyn $
          throw $ BadArgType (typeRep (Proxy @c)) (dynTypeRep xDyn)

-- | Just ignore metadata for this one.
instance (GGetInputValues f) => GGetInputValues (M1 i t f) where
  gGetInputValues = M1 <$> gGetInputValues

-- * Functions

-- | Runs a transaction on its inputs, with some basic information about
-- the context.  This is written as a 'modify' call, so that the actual
-- transaction running can be pure functions.
runTransaction :: 
  (GetInputValues inputs, HasEscrowIDs inputs, Typeable a, Show a) =>
  Transaction inputs a -> Inputs -> 
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
          signedBy = txKey,
          result = throw e :: Void
        }

-- | A summary monad-running function.  You start with no escrows and the
-- next escrow ID is the transaction ID.
runFaeContract :: TransactionID -> PublicKey -> FaeContract Naught a -> a
runFaeContract (ShortContractID dig) txKey =
  flip runReader txKey .
  fmap fst . runWriterT .
  flip evalStateT Escrows{escrowMap = Map.empty, nextID = dig} .
  runCoroutine

-- | Running the actual 'Transaction' as a function.  We don't convert to
-- 'InternalT' or 'ConcreteContract' because transactions are rather
-- unique: the handling of the arguments and the escrows is quite
-- different.
transaction :: 
  (GetInputValues input, HasEscrowIDs input, Typeable a, Show a) =>
  TransactionID ->
  Bool -> 
  Inputs -> 
  Transaction input a -> 
  Storage -> FaeContract Naught Storage
transaction txID isReward args f storage = do
  (input, storage', inputOutputs) <- runInputSpec args isReward storage
  (result, outputs) <- doTX f input
  signedBy <- sender
  return $ storage' 
    & _getStorage . at txID ?~ TransactionEntry{..}
    & _txLog %~ cons txID

-- | Runs the transaction and checks that it didn't leave any escrows open.
-- That would allow value to be destroyed, possibly by accident, which is
-- bad.
doTX :: Transaction input a -> input -> FaeContract Naught (a, Outputs)
doTX f input = do
  (result, outputsL) <- listen $ getFae $ f input
  let outputs = intMapList outputsL
  escrows <- use _escrowMap
  unless (Map.null escrows) $ throw OpenEscrows
  return (result, outputs)

-- | Takes the list of contract IDs with input strings and forms the input
-- object out of their results, taking care to resolve its escrow locators.
runInputSpec :: 
  forall input.
  (GetInputValues input, HasEscrowIDs input) =>
  Inputs -> Bool -> Storage -> 
  FaeContract Naught (input, Storage, InputOutputs)
runInputSpec args isReward storage = do
  triple <- runInputContracts (Proxy @input) isReward storage args
  let (input0, unused) = getInputValues $ triple ^. _1
  unless (null unused) $ throw TooManyInputs
  -- We don't have to be careful about self-referencing here because we
  -- already checked it in 'runInputContract'
  input <- runTXEscrows $ resolveEscrowLocators input0 input0 
  return (triple & _1 .~ input)
 
-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts ::
  (GetInputValues input, HasEscrowIDs input) =>
  Proxy input ->
  Bool ->
  Storage ->
  Inputs ->
  FaeContract Naught ([Dynamic], Storage, InputOutputs)
runInputContracts p isReward storage args = do
  runIdentityT $ flip execStateT ([], storage, Map.empty) $ 
    forM_ args $ modifyM . uncurry (runInputContract p isReward)
  where modifyM f = get >>= lift . IdentityT . f >>= put

-- | Runs a single input contract.  It is here that the rewards are
-- created, if any.  This function uses the list of previous results to
-- partially construct an input object for looking up escrow locators in
-- the present contract; this only works because of laziness.
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


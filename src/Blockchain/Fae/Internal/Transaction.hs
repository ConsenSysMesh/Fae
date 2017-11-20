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
import Blockchain.Fae.Internal.Versions

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Trans.Identity

import Data.Dynamic
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Typeable
import Data.Void

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import GHC.Generics hiding (to)
import qualified GHC.Generics as Gen (to)

import Numeric.Natural

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
type InputOutputVersions = InputOutputVersionsT AbstractContract
-- | Likewise
type FaeStorage = FaeStorageT AbstractContract
-- | Likewise
type TransactionEntry = TransactionEntryT AbstractContract

-- | How inputs are provided to transactions.
type Inputs = [(ContractID, String)]

-- | Transactions, though similar to contracts in many internal ways,
-- differ greatly in their inputs and outputs.  The argument of type 'a' is
-- constructed from the return values of the input contracts according to
-- its 'GetInputValues' instance.  The return value need not and can not
-- contain escrows (or rather, the escrow IDs it contains will not be
-- transferred anywhere).
type Transaction a b = a -> FaeTX b

-- | This monad is a little different from 'FaeTX' in that it is built on
-- 'FaeStorage', meaning it has direct access to the storage and also to
-- 'IO'.  Needless to say, we don't want any of that near user-defined
-- contracts.
type TXStorageM = FaeContractT Naught FaeStorage

-- | Exception type
data TransactionException =
  NotEnoughInputs |
  TooManyInputs |
  BadInput ContractID |
  OpenEscrows 
  deriving (Typeable, Show)

{- Typeclasses -}

-- | This class controls how a type is constructed from
-- a heterogeneously-typed list of input return values.  Its default
-- instance, for any 'Generic' type, simply assigns each field of a product
-- type from successive values in the list, failing if they don't all match
-- or there are extras on one side.  For the moment, the member function is
-- not exported, so you can't write your own implementations; however, you
-- do need to @instance GetInputValues <your type>@ for any 'a' you choose
-- to use.
class GetInputValues a where
  getInputValues :: [Dynamic] -> (a, [Dynamic])
  default getInputValues :: 
    (Generic a, GGetInputValues (Rep a)) => [Dynamic] -> (a, [Dynamic])
  getInputValues = runState $ Gen.to <$> gGetInputValues

-- | Generic helper class
class GGetInputValues f where
  -- | This is in the 'State' monad, rather than having the same signature as
  -- 'getInputValues', because we need to walk through the list of dynamic
  -- inputs and progressively remove values from it.  At the same time, we
  -- need to know if there were leftovers.
  gGetInputValues :: State [Dynamic] (f p)

{- Instances -}

-- | Of course
instance Exception TransactionException

-- | Empty types not only have no input values, it is insulting to suggest
-- that they might.
instance GetInputValues Void where
  getInputValues _ = (throw TooManyInputs, [])
-- | Generic instance
instance GetInputValues ()
-- | Generic instance
instance GetInputValues Bool where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Char where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Int where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Integer where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Float where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Double where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues Natural where
  getInputValues = defaultGetInputValues
-- | Default instance
instance GetInputValues PublicKey where
  getInputValues = defaultGetInputValues
-- | Lists are read as a single value, because it makes no sense to expect
-- an indefinitely long input when they have to be given as literals.
instance (Typeable a) => GetInputValues [a] where
  getInputValues = defaultGetInputValues
-- | Default instance
instance (Typeable a) => GetInputValues (Maybe a) where
  getInputValues = defaultGetInputValues
-- | Default instance
instance (Typeable a, Typeable b) => GetInputValues (Either a b) where
  getInputValues = defaultGetInputValues
-- | Generic instance.
instance 
  (GetInputValues a, GetInputValues b, Typeable a, Typeable b) => 
  GetInputValues (a, b)
-- | Default instance
instance 
  (Typeable argType, Typeable valType) => 
  GetInputValues (EscrowID argType valType) where

  getInputValues = defaultGetInputValues
-- | Generic instance.
instance 
  (
    GetInputValues a, GetInputValues b, GetInputValues c, 
    Typeable a, Typeable b, Typeable c

  ) => 
  GetInputValues (a, b, c)

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
-- /not/ do a recursive call.
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
  forall a inputs.
  (GetInputValues inputs, HasEscrowIDs inputs, Typeable a, Show a) =>
  Transaction inputs a -> Inputs -> 
  TransactionID -> Signers -> Bool -> 
  FaeStorage ()
runTransaction f inputArgs txID signers isReward = runFaeContract txID signers $ do
  liftFaeContract $ _getStorage . at txID ?= 
    TransactionEntry
    {
      inputOutputs = Map.empty,
      outputs = IntMap.empty,
      signers,
      result = undefined :: a
    }
  input <- runInputSpec inputArgs isReward 
  (result, outputs) <- handleAll (\e -> return (throw e, throw e)) $ do
    (result, outputsL) <- hoistFaeContract $ listen $ getFae $ f input
    escrows <- use _escrowMap
    unless (Map.null escrows) $ throw OpenEscrows
    return (result, intMapList outputsL)
  liftFaeContract $ do
    _getStorage . at txID %= 
      fmap (\TransactionEntry{inputOutputs} -> TransactionEntry{..})
    _txLog %= cons txID

-- | Takes the list of contract IDs with input strings and forms the input
-- object out of their results.
runInputSpec :: 
  (GetInputValues input, HasEscrowIDs input) =>
  Inputs -> Bool -> 
  TXStorageM input
runInputSpec args isReward = do
  inputsL <- runInputContracts args
  (input, unused) <- hoistFaeContract $ getInputValues <$> withReward inputsL 
  unless (null unused) $ throw TooManyInputs
  return input

  where
    withReward 
      | isReward = \inputs -> do
          eID <- newEscrow [] rewardEscrow
          return $ toDyn eID : inputs
      | otherwise = return 
 
-- | Runs all the input contracts in a state monad recording the
-- progressively increasing set of outputs.
runInputContracts :: Inputs -> TXStorageM [Dynamic]
runInputContracts args = 
  fmap fst $ flip execStateT ([], Map.empty) $ forM_ args $ 
    modifyM . uncurry runInputContract
  where modifyM f = get >>= lift . f >>= put

-- | Runs a single input contract.  It is here that the rewards are
-- created, if any.
runInputContract ::
  ContractID -> String ->
  ([Dynamic], VersionMap) -> TXStorageM ([Dynamic], VersionMap)
runInputContract cID arg (results, vers) = do
  ConcreteContract fAbs <- liftFaeContract $ use $
    at cID . defaultLens (throw $ BadInput cID)
  let inputError e = return (throw e, throw e, vers)
  (ioVersions, result, vers') <- handleAll inputError $ do
    ((gAbsM, (result, vMap)), outputsL) <- 
      hoistFaeContract $ listen $ fAbs (arg, vers)
    let 
      iOutputs = intMapList outputsL
      inputVersions = fmap dynTypeRep vMap
    liftFaeContract $ at cID .= gAbsM
    return (InputOutputVersions{..}, result, vers `Map.union` vMap)
  txID <- view _thisTXID
  liftFaeContract $ _getStorage . at txID %= 
    fmap (_inputOutputs . at (shorten cID) ?~ ioVersions)
  censor (const []) $ return (results |> result, vers')

defaultGetInputValues :: forall a. (Typeable a) => [Dynamic] -> (a, [Dynamic])
defaultGetInputValues (xDyn : rest) = (x, rest) where
  x = fromDyn xDyn $
    throw $ BadArgType (typeRep (Proxy @a)) (dynTypeRep xDyn)
defaultGetInputValues [] = throw NotEnoughInputs



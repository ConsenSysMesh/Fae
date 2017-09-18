module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Monads

import Control.Monad.Coroutine
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Writer

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Dynamic
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Sequence (Seq)

data Reward = Reward
data RewardToken = Token
type RewardEscrowID = EscrowID RewardToken Reward 

rewardEscrow :: (MonadFae argType valType m) => Contract m RewardToken Reward
rewardEscrow Token = spend Reward

claimReward :: RewardEscrowID -> AnyFae ()
claimReward eID = do
  Reward <- useEscrow eID Token
  return ()

data InputArg =
  LiteralArg Dynamic |
  TrustedArg Int

runTransaction :: 
  forall a result.
  (Typeable a) =>
  TransactionID -> PublicKey -> Bool ->
  Seq (ContractID, InputArg) -> 
  Transaction a -> FaeBlock a
runTransaction txID txKey isReward inputArgs f = state $ 
  runFaeContract txID txKey . 
  transaction txID isReward inputArgs f 

runFaeContract :: TransactionID -> PublicKey -> FaeContractStateT Naught a -> a
runFaeContract txID txKey =
  fst .
  (\r s m -> evalRWS m r s) txKey (txID, 0) .
  unWrapped . 
  flip evalStateT Map.empty .
  runCoroutine

transaction :: 
  (Typeable a) =>
  TransactionID ->
  Bool -> 
  Seq (ContractID, InputArg) -> 
  Transaction a -> 
  Storage -> FaeContractStateT Naught (a, Storage)
transaction txID isReward inputArgs f storage = do
  (inputs0, storage', inputOutputs) <- runInputContracts inputArgs storage
  inputs <- withReward inputs0
  (result0, outputsL) <- listen $ getFae $ f (Inputs inputs)
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
  Seq (ContractID, InputArg) ->
  Storage ->
  FaeContractStateT s (Seq Dynamic, Storage, InputOutputs)
runInputContracts inputArgs storage = 
  (\x s f -> foldl f x s) (return (Seq.empty, storage, Map.empty)) inputArgs $
  \accM (cID, arg) -> do
    (results, storage, inputOutputs) <- accM
    let 
      realArg = case arg of
        LiteralArg xDyn -> xDyn
        TrustedArg i -> 
          fromMaybe (throw $ BadChainedInput cID i) (results Seq.!? i) 
      ConcreteContract fAbs = 
        storage ^. 
        at cID . defaultLens (throw $ BadInput cID) . 
        _abstractContract
    ((gAbsM, result), outputsL) <- listen $ fAbs realArg
    censor (const []) $ return
      (
        results |> result,
        storage & at cID %~ liftM2 (_abstractContract .~) gAbsM,
        inputOutputs & at (shorten cID) ?~ intMapList outputsL
      )

type instance Index Storage = ContractID
type instance IxValue Storage = TrustContract
instance Ixed Storage 
instance At Storage where
  at cID@(JustTransaction txID) = throw (BadContractID cID)

  at cID@(TransactionOutput txID i) =
    _getStorage .
    at txID .
    defaultLens (throw $ BadTransactionID txID) .
    _outputs .
    at i

  at cID@(InputOutput txID sID i) = 
    _getStorage .
    at txID .
    defaultLens (throw $ BadTransactionID txID) .
    _inputOutputs .
    at sID .
    defaultLens (throw $ BadInputID sID) .
    at i

intMapList :: [a] -> IntMap a
intMapList = IntMap.fromList . zip [0 ..]

module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Lens

import Control.Monad.Reader

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Dynamic
import Data.Foldable
import Data.Maybe

type Transaction a = Fae () () a

data Reward = Reward

runTransaction :: 
  forall a result.
  (Typeable a, FaeReturn a a) =>
  TransactionID -> [(ContractID, Dynamic)] -> PublicKey -> Bool ->
  Transaction a -> FaeBlock ()
runTransaction txID inputArgs transactionKey isReward f = 
  handleAll (setException txID) $ do
    let 
      contractID = JustTransaction txID
      inputIDs = map fst inputArgs
    inputOutputData <- zip inputIDs <$> 
      traverse (getInput txID transactionKey) inputArgs
    let 
      entryID = EntryID $ digest txID

      escrowID :: EscrowID () Reward
      escrowID = EscrowID entryID

      rewardEscrow :: Escrow () Reward
      rewardEscrow = error "Rewards not implemented"

      inputs 
        | isReward = s |> (contractID, toDyn escrowID)
        | otherwise = s
        where s = Seq.fromList $ map (_2 %~ retVal) inputOutputData
      outputID = TransactionOutput txID
      contractData = ContractData{..}

      newEscrows = catMaybes $ map (newEscrow . snd) inputOutputData
      escrows
        | isReward = Map.fromList $ (entryID, toDyn rewardEscrow) : newEscrows
        | otherwise = Map.fromList newEscrows
      accum = ()
      stateData = StateData{spent = False, ..}
      
    let FaeContract c = concrete stateData f ()
    let outputData = runReader c contractData 

    traverse (uncurry (assign.at) . (_2 %~ updatedContract)) inputOutputData
    _getStorage . at txID ?=
      TransactionEntry
      {
        returnValue = toDyn (retVal outputData :: a),
        outputs = seqToOutputs $ outputContracts outputData,
        inputContracts = 
          Map.fromList $ 
          map (\p -> p & _1 %~ shorten & _2 %~ seqToOutputs . outputContracts) 
          inputOutputData
      }
     
  where
    seqToOutputs = IntMap.fromList . zip [0 ..] . toList
    setException txID e = 
      _getStorage . at txID ?= 
        TransactionEntry
        {
          returnValue = toDyn e,
          outputs = IntMap.empty,
          inputContracts = Map.empty
        }

getInput :: 
  TransactionID -> PublicKey -> (ContractID, Dynamic) -> 
  FaeBlock (OutputData Dynamic)
getInput txID transactionKey (contractID, arg) = do
  c <- use $ at contractID . defaultLens (throw $ BadContractID contractID) 
  let 
    inputs = Seq.empty
    outputID = InputOutput txID (shorten contractID)
    contractData = ContractData{..}
  let FaeContract f = c arg
  return $ runReader f contractData 

type instance Index Storage = ContractID
type instance IxValue Storage = AbstractContract
instance Ixed Storage 
instance At Storage where
  at cID@(JustTransaction _) = 
    throw $ BadContractID cID

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
    _inputContracts .
    at sID .
    defaultLens (throw $ BadInputID sID) .
    at i


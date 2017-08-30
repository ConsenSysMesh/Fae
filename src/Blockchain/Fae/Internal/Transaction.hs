module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Lens
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Reward

import Control.Monad.Fix
import Control.Monad.Reader

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Dynamic
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq)

type Transaction a = Fae () () a

data InputArg =
  LiteralArg Dynamic |
  TrustedArg Int

runTransaction :: 
  forall a result.
  (Typeable a, FaeReturn a a) =>
  TransactionID -> Seq (ContractID, InputArg) -> PublicKey -> Bool ->
  Transaction a -> FaeBlock ()
runTransaction txID inputArgs transactionKey isReward f = 
  handleAll (setException txID) $ do
    let 
      contractID = JustTransaction txID
      inputIDs = fst <$> inputArgs
    inputOutputData <- mfix $ \outputData ->
      fmap (Seq.zip inputIDs) $
      sequence $ Seq.zipWith (getInput txID transactionKey) 
      (Seq.inits $ (_2 %~ retVal) <$> outputData) inputArgs
    let
      entryID = EntryID $ digest txID

      escrowID :: RewardEscrowID
      escrowID = EscrowID entryID

      rewardState = 
        StateData
        {
          escrows = Map.empty,
          accum = (),
          spent = False
        }

      rewardEscrow :: RewardEscrow
      rewardEscrow = Escrow $ concrete rewardState $ do
        Token <- ask
        return Reward
    let 
      inputs 
        | isReward = s |> (contractID, toDyn escrowID)
        | otherwise = s
        where s = (_2 %~ retVal) <$> inputOutputData
      outputID = TransactionOutput txID
      contractData = ContractData{..}

      newEscrows = catMaybes $ (newEscrow . snd) <$> toList inputOutputData
      escrows
        | isReward = Map.fromList $ (entryID, toDyn rewardEscrow) : newEscrows
        | otherwise = Map.fromList newEscrows
      accum = ()
      stateData = StateData{spent = False, ..}

      inputArg = ()
      inputEscrow = Nothing
      
    let FaeContract c = concrete stateData f InputData{..}
    let outputData = runReader c contractData 

    traverse
      (\(cID, OutputData{..}) -> 
        modifying (at cID) 
          (\pM -> do
            uC <- updatedContract
            p <- pM
            return $ p & _1 .~ uC
          )
      )
      inputOutputData
    
    _getStorage . at txID ?=
      TransactionEntry
      {
        returnValue = toDyn (retVal outputData :: a),
        outputs = seqToOutputs $ outputContracts outputData,
        inputContracts = 
          Map.fromList $ toList $
          fmap (\p -> p & _1 %~ shorten & _2 %~ seqToOutputs . outputContracts) 
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
  TransactionID -> PublicKey -> 
  Seq (ContractID, Dynamic) -> (ContractID, InputArg) -> 
  FaeBlock (OutputData Dynamic)
getInput txID transactionKey prevOuts (contractID, givenArg) = do
  (c, trusts) <- 
    use $ at contractID . defaultLens (throw $ BadContractID contractID) 
  let 
    inputs = Seq.empty
    outputID = InputOutput txID (shorten contractID)
    inputArg =
      case givenArg of
        LiteralArg x -> x
        TrustedArg i
          | Just (prevCID, x) <- prevOuts Seq.!? i,
            shorten prevCID `elem` trusts
            -> x
          | otherwise -> throw $ BadChainedInput contractID i
    contractData = ContractData{..}
    inputEscrow = Nothing
  let FaeContract f = c InputData{..}
  return $ runReader f contractData 

type instance Index Storage = ContractID
type instance IxValue Storage = TrustContract
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


module Blockchain.Fae.Internal.Transaction where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto hiding (signer)
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Monads
import Blockchain.Fae.Internal.Lens

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Dynamic

newtype Transaction a = Transaction { getTransaction :: Contract () () a }

runTransaction :: 
  (Typeable a) =>
  Integer -> PublicKey -> Transaction a -> FaeBlock ()
runTransaction txNum sender x = handleAll (setException txID) $ 
  FaeBlock $ do
    contracts <- get
    let 
      newContracts = 
        runTX sender contracts contractData $ 
        concrete stateData (getTransaction x) ()
    put newContracts

  where
    stateData =
      StateData
      {
        escrows = Escrows $ Map.empty,
        accum = ()
      }
    contractData =
      ContractData
      {
        callPath = nullPath,
        contractID = txID,
        escrowArg = Nothing
      }
    nullPath = CallPath Seq.empty
    txID = ContractID nullPath txNum
    setException txID e = FaeBlock $ at txID ?= const (return $ toDyn e)


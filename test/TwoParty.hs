import Blockchain.Fae
import Blockchain.Fae.Contracts
import Blockchain.Fae.Internal

import Control.Monad.State

import Data.Dynamic
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq

import GHC.Generics (Generic)

import Data.Void

instance Digestible Int

main :: IO ()
main = do
  pubKey <- fromJust . public <$> newPrivateKey
  pubKeyA <- fromJust . public <$> newPrivateKey
  pubKeyB <- fromJust . public <$> newPrivateKey
  (resultA, resultB) <- flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty (startSwapTX pubKeyA pubKeyB) 
    runTransaction txID2 pubKeyA False Seq.empty (offer2TX A cID)
    runTransaction txID3 pubKeyB False Seq.empty (offer2TX B cID)
    runTransaction txID4 pubKeyA False args voteTX
    runTransaction txID5 pubKeyB False args voteTX
    resultA <- runTransaction txID6 pubKeyA False claimArgsB claimTX
    resultB <- runTransaction txID7 pubKeyB False claimArgsA claimTX
    return (resultA, resultB)
  putStrLn $ "A got: " ++ resultA
  putStrLn $ "B got: " ++ resultB

  where
    args = Seq.singleton (cID, "True")
    claimArgsA = claimArgs cIDA
    claimArgsB = claimArgs cIDB
    claimArgs cIDX = Seq.fromList
      [
        (cID, "True"),
        (cIDX, "()")
      ]
    cID = TransactionOutput txID1 0
    cIDA = TransactionOutput txID2 0
    cIDB = TransactionOutput txID3 0
    txID1 = ShortContractID $ digest (1 :: Int)
    txID2 = ShortContractID $ digest (2 :: Int)
    txID3 = ShortContractID $ digest (3 :: Int)
    txID4 = ShortContractID $ digest (4 :: Int)
    txID5 = ShortContractID $ digest (5 :: Int)
    txID6 = ShortContractID $ digest (6 :: Int)
    txID7 = ShortContractID $ digest (7 :: Int)

startSwapTX :: PublicKey -> PublicKey -> Transaction Void ()
startSwapTX pubKeyA pubKeyB _ = twoPartySwap pubKeyA pubKeyB

offer2TX :: TwoParties -> ContractID -> Transaction Void ()
offer2TX p cID _ = offer2 p ("Hello from " ++ show p) (shorten cID)

voteTX :: Transaction Bool ()
voteTX _ = return ()

claimTX :: 
  Transaction (Maybe TwoPartyToken, TXEscrowID TwoPartyToken String) String
claimTX (tokenM, pID) = useTXEscrow pID $ fromJust tokenM


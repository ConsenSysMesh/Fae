import Blockchain.Fae
import Blockchain.Fae.Internal

import Control.Monad.State

import Data.Dynamic
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GHC.Generics (Generic)

instance Digestible Int
instance GetInputValues ()
instance (Typeable a, Typeable b) => GetInputValues (a, b)

main :: IO ()
main = do
  result <- flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty createContract1TX
    runTransaction txID2 pubKey False Seq.empty (createContract2TX cID1)
    runTransaction txID3 pubKey False runArgs runContractTX
  putStrLn result

  where
    runArgs = Seq.fromList
      [
        (cID1, LiteralArg $ toDyn "Hello, world!"),
        (cID2, TrustedArg 0)
      ]
    cID1 = TransactionOutput txID1 0
    cID2 = TransactionOutput txID2 0
    txID1 = ShortContractID $ digest (1 :: Int)
    txID2 = ShortContractID $ digest (2 :: Int)
    txID3 = ShortContractID $ digest (3 :: Int)
    pubKey = undefined

createContract1TX :: Transaction () ()
createContract1TX = \() -> newContract [] [] c
  where
    c :: Contract String (EscrowID () String)
    c s = do
      eID <- newEscrow [] $ \() -> spend s
      spend eID

createContract2TX :: ContractID -> Transaction () ()
createContract2TX cID = \() -> newContract [] [shorten cID] c
  where
    c :: Contract (EscrowID () String) String
    c eID = do
      s <- useEscrow eID ()
      spend s

runContractTX :: Transaction (EscrowID () String, String) String
runContractTX = return . snd


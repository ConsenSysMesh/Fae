import Blockchain.Fae
import Blockchain.Fae.Internal

import Control.Monad.State

import Data.Dynamic
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Void

import GHC.Generics (Generic)

instance Digestible Int

main :: IO ()
main = do
  result <- flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty createContractTX
    runTransaction txID2 pubKey False runArgs runContractTX
  putStrLn result

  where
    runArgs = Seq.singleton $
      (TransactionOutput txID1 0, toDyn "Hello, world!")
    txID1 = ShortContractID $ digest (1 :: Int)
    txID2 = ShortContractID $ digest (2 :: Int)
    pubKey = undefined

createContractTX :: Transaction Void ()
createContractTX = \_ -> newContract [] c
  where
    c :: Contract String (EscrowID () String)
    c s = do
      eID <- newEscrow [] $ \() -> spend s
      spend eID

runContractTX :: Transaction (EscrowID () String) String
runContractTX eID = useEscrow eID ()


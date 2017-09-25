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
instance (Typeable a) => GetInputValues (Identity a)

main :: IO ()
main = do
  result <- flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty createContractTX
    runTransaction txID2 pubKey False runArgs runContractTX
  putStrLn result

  where
    runArgs = Seq.singleton $
      (TransactionOutput txID1 0, LiteralArg $ toDyn "Hello, world!")
    txID1 = ShortContractID $ digest (1 :: Int)
    txID2 = ShortContractID $ digest (2 :: Int)
    pubKey = undefined

createContractTX :: Transaction () ()
createContractTX = \() -> newContract [] [] c
  where
    c :: Contract String (EscrowID () String)
    c s = do
      eID <- newEscrow [] $ \() -> spend s
      spend eID

runContractTX :: Transaction (Identity (EscrowID () String)) String
runContractTX (Identity eID) = useEscrow eID ()


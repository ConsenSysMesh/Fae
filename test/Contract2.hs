import Blockchain.Fae
import Blockchain.Fae.Internal

import Control.Monad.IO.Class
import Control.Monad.State

import Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

instance Digestible Int
instance GetInputValues ()
instance GetInputValues (String, String)

main :: IO ()
main = 
  flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty (createContractTX [])
    runTransaction txID2 pubKey False Seq.empty (createContractTX [cID1])
    result <- runTransaction txID3 pubKey False runArgs runContractTX
    liftIO $ putStrLn result

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

createContractTX :: [ContractID] -> Transaction () ()
createContractTX trusts = \() -> newContract [] (map shorten trusts) c
  where
    c :: Contract String String
    c = spend

runContractTX :: Transaction (String, String) String
runContractTX = return . snd


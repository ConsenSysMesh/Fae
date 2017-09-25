import Blockchain.Fae
import Blockchain.Fae.Internal

import Control.Monad.IO.Class
import Control.Monad.State

import Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import GHC.Generics (Generic)

newtype StringArg = StringArg { getStringArg :: String } deriving (Generic)

instance Digestible Int
instance GetInputValues ()
instance GetInputValues StringArg

main :: IO ()
main = 
  flip evalStateT (Storage Map.empty) $ do
    runTransaction txID1 pubKey False Seq.empty createContractTX
    result <- runTransaction txID2 pubKey False runArgs runContractTX
    storage <- get
    let 
      stored = 
        storage ^. _getStorage . at txID2 . defaultLens undefined . _result .
        to (flip fromDyn undefined)
    liftIO $ putStrLn $ "Result: " ++ result
    liftIO $ putStrLn $ "Stored: " ++ stored

  where
    runArgs = Seq.singleton $
      (TransactionOutput txID1 0, LiteralArg $ toDyn "Hello, world!")
    txID1 = ShortContractID $ digest (1 :: Int)
    txID2 = ShortContractID $ digest (2 :: Int)
    pubKey = undefined

createContractTX :: Transaction () ()
createContractTX = \() -> newContract [] [] c
  where
    c :: Contract String String
    c = spend

runContractTX :: Transaction StringArg String
runContractTX = return . getStringArg


import Blockchain.Fae
import Blockchain.Fae.Internal

import Control.Monad.State

import Data.Dynamic
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

instance Digestible ()
instance GetInputValues ()

main :: IO ()
main = do
  (result, storage) <- flip runStateT (Storage Map.empty) $
    runTransaction txID pubKey False Seq.empty helloWorldTX
  let 
    stored = 
      storage ^. _getStorage . at txID . defaultLens undefined . _result .
      to (flip fromDyn undefined)
  putStrLn $ "Result: " ++ result
  putStrLn $ "Stored: " ++ stored

  where
    txID = ShortContractID $ digest ()
    pubKey = undefined

helloWorldTX :: Transaction () String
helloWorldTX = \() -> return "Hello, world!"


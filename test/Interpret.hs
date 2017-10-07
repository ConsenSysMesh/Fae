import Blockchain.Fae.Internal
import Control.Monad.State
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
  txNames <- getArgs
  (_, Storage storage) <- 
    flip runStateT (Storage Map.empty) $ 
      getFaeStorage $ mapM (interpretTX False) txNames
  print $ Map.map (\(TransactionEntry _ _ result) -> show result) storage

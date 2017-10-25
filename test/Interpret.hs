{-# LANGUAGE TupleSections #-}
import Blockchain.Fae.Internal
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
  txNames <- getArgs
  txs <- flip evalStateT (Storage Map.empty []) $ getFaeStorage $ do
    interpretTXs $ map (,False) txNames
    showTransactions   
  putStrLn $ intercalate "\n\n" txs

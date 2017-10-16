{-# LANGUAGE TupleSections #-}
import Blockchain.Fae.Internal
import Control.Monad.State
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
  txNames <- getArgs
  (_, Storage storage) <- 
    flip runStateT (Storage Map.empty) $ 
      getFaeStorage $ interpretTXs $ 
        zipWith (\s n -> (s, digest n, False)) txNames [0 :: Int ..]
  print $ Map.map (\(TransactionEntry _ _ result) -> show result) storage

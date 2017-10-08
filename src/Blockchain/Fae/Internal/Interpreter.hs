module Blockchain.Fae.Internal.Interpreter where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.Monad 

import Language.Haskell.Interpreter

interpretTXs :: [(String, Bool)] -> FaeStorage ()
interpretTXs txSrcs = do
  result <- runInterpreter $ do
    loadModules $ map fst txSrcs 
    forM txSrcs $ \(txSrc, isReward) -> do
      setImports [txSrc, "Blockchain.Fae.Internal", "Data.Sequence", "Prelude", "Data.Dynamic"]
      interpret 
        ("runTransaction txID pubKey " ++ show isReward ++ " (fromList inputs) body")
        (as :: FaeStorage ())
  either throw sequence_ result

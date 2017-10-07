module Blockchain.Fae.Internal.Interpreter where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Data.Typeable

import Language.Haskell.Interpreter

interpretTX :: Bool -> String -> FaeStorage ()
interpretTX isReward txSrc = do
  result <- runInterpreter $ do
    loadModules [txSrc]
    setImports [txSrc, "Blockchain.Fae.Internal", "Data.Sequence", "Prelude", "Data.Dynamic"]
    interpret 
      ("runTransaction txID pubKey " ++ show isReward ++ " (fromList inputs) body")
      (as :: FaeStorage ())
  either throw id result

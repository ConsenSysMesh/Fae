module Blockchain.Fae.Internal.Interpreter where

import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.Monad 

import Data.List

import Language.Haskell.Interpreter

interpretTXs :: [(String, Bool)] -> FaeStorage ()
interpretTXs txSpecs = do
  result <- runInterpreter $ do
    loadModules txSrcs
    setImportsQ $ map (,Nothing) pkgModules ++ map (\s -> (s, Just s)) txSrcs
    mapM (flip interpret (as :: FaeStorage ()) . uncurry runTX) txSpecs
  either throw sequence_ result

  where
    txSrcs = map fst txSpecs
    pkgModules = 
      [
        "Prelude",
        "Data.Dynamic",
        "Data.Sequence",
        "Blockchain.Fae.Internal"
      ]
    runTX s isReward =
      intercalate " "
        [
          "runTransaction",
          s ++ ".txID",
          s ++ ".pubKey",
          show isReward,
          parens $ "fromList " ++ s ++ ".inputs",
          s ++ ".body"
        ]

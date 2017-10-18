module Blockchain.Fae.Internal.Interpreter where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.Monad 

import Data.List

import Language.Haskell.Interpreter

interpretTXs :: [(String, Digest, Bool)] -> FaeStorage ()
interpretTXs txSpecs = do
  actions <- runInterpreter $ do
    loadModules txSrcs
    setImportsQ $ map (,Nothing) pkgModules ++ map (\s -> (s, Just s)) txSrcs
    mapM (flip interpret (as :: FaeStorage ()) . runTX) txSpecs  
  either reportErr sequence_ actions

  where
    reportErr (WontCompile ghcErrors) = error $ errMsg $ head ghcErrors
    reportErr e = throw e
    txSrcs = map (\(x,_,_) -> x) txSpecs
    pkgModules = 
      [
        "Prelude",
        "Data.Dynamic",
        "Blockchain.Fae.Internal"
      ]
    runTX (s, dig, isReward) =
      intercalate " "
        [
          "runTransaction",
          parens $ "read " ++ show (show dig),
          s ++ ".txID",
          s ++ ".pubKey",
          show isReward,
          s ++ ".inputs",
          s ++ ".body"
        ]

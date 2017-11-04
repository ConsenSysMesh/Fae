module Blockchain.Fae.Internal.TX where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.Monad 
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

import GHC.Generics

import Language.Haskell.Interpreter as Int 

{- Types -}

data TX =
  TX
  {
    txID :: TransactionID,
    pubKey :: PublicKey
  }
  deriving (Generic)

type FaeInterpret = InterpreterT FaeStorage

{- Instances -}

instance Serialize TX
instance Digestible TX

{- Functions -}

interpretTX :: Bool -> TX -> FaeInterpret ()
interpretTX isReward TX{..} = do
  Int.set [languageExtensions := languageExts]
  loadModules [txSrc]
  setImportsQ $ (txSrc, Just txSrc) : map (,Nothing) pkgModules 
  run <- interpret 
    ("runTransaction  " ++ qualified "body" ++ " " ++ qualified "inputs")
    (as :: TransactionID -> PublicKey -> Bool -> FaeStorage ())
  lift $ run txID pubKey isReward 
  where
    txSrc = "Blockchain.Fae.Transactions.TX" ++ show txID
    pkgModules = 
      [
        "Blockchain.Fae.Internal",
        "Data.Dynamic"
      ]
    languageExts =
      [
        DeriveDataTypeable,
        DeriveGeneric,
        OverloadedStrings
      ]
    qualified varName = txSrc ++ "." ++ varName

runFaeInterpret :: FaeInterpret a -> IO a
runFaeInterpret = 
  fmap (either reportErr id) .
  flip evalStateT (Storage Map.empty []) . 
  getFaeStorage . 
  runInterpreter

  where
    reportErr (WontCompile ghcErrors) = error $ errMsg $ head ghcErrors
    reportErr e = throw e


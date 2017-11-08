module Blockchain.Fae.Internal.TX where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.DeepSeq

import Control.Monad 
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

import GHC.Generics

import Language.Haskell.Interpreter as Int 

import System.FilePath

{- Types -}

data TX =
  TX
  {
    txID :: TransactionID,
    inputs :: Inputs,
    pubKey :: PublicKey
  }
  deriving (Generic)

type FaeInterpret = InterpreterT FaeStorage

{- Instances -}

instance Serialize TX
instance Digestible TX
instance NFData TX

{- Functions -}

interpretTX :: Bool -> TX -> FaeInterpret ()
interpretTX isReward TX{..} = handle (liftIO . fixGHCErrors) $ do
  Int.set 
    [
      languageExtensions := languageExts,
      searchPath := thisTXPath
    ]
  loadModules [txSrc]
  setImportsQ $ (txSrc, Just txSrc) : map (,Nothing) pkgModules 
  run <- interpret 
    ("runTransaction " ++ qualified "body")
    infer
    --(as :: Inputs -> TransactionID -> PublicKey -> Bool -> FaeStorage ())
  lift $ run inputs txID pubKey isReward 
  where
    fixGHCErrors (WontCompile []) = error "Compilation error"
    fixGHCErrors (WontCompile (ghcE : _)) = error $ errMsg ghcE
    fixGHCErrors (UnknownError e) = error e
    fixGHCErrors (NotAllowed e) = error e
    fixGHCErrors (GhcException e) = error e
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
    thisTXPath = 
      [
        ".",
        "Blockchain" </> "Fae" </> "Transactions" </> 
          ("TX" ++ show txID) </> "private"
      ]
    qualified varName = txSrc ++ "." ++ varName

runFaeInterpret :: FaeInterpret a -> IO a
runFaeInterpret = 
  fmap (either throw id) .
  flip evalStateT (Storage Map.empty []) . 
  getFaeStorage . 
  runInterpreter


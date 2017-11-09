{- |
Module: Blockchain.Fae.Internal.TX
Description: Transaction interpreter
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides the function that interprets transactions given as source code.
-}
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

-- * Types

-- | This is the data type that identifies a transaction in a block; the
-- actual transaction function is only known at the time of interpretation.
data TX =
  TX
  {
    txID :: TransactionID,
    inputs :: Inputs,
    pubKey :: PublicKey
  }
  deriving (Generic)

-- | This builds on 'FaeStorage' because the interpreter has to access the
-- storage as part of 'runTransaction'.
type FaeInterpret = InterpreterT FaeStorage

{- Instances -}

-- | Default instance
instance Serialize TX
-- | Default instance
instance Digestible TX
-- | Default instance
instance NFData TX

-- * Functions

-- | Interprets a transaction, looking it up as a module named after its
-- transaction ID; the first argument is whether or not the transaction
-- gets a reward.  We set up the module search path carefully so that this
-- transaction can effectively import both its own other modules, and those
-- of other transactions.  The first transaction is very slow because the
-- interpreter has to initialize, but subsequent ones are acceptably fast.
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

-- | Runs the interpreter.
runFaeInterpret :: FaeInterpret a -> IO a
runFaeInterpret = 
  fmap (either throw id) .
  flip evalStateT (Storage Map.empty []) . 
  getFaeStorage . 
  runInterpreter


{- |
Module: Blockchain.Fae.Internal.TX
Description: Transaction interpreter
Copyright: (c) Ryan Reich, 2017
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module provides the function that interprets transactions given as
source code.  The interpreter enables the following extensions globally;
this is the set of extensions I consider to be both generally harmless and
indispensable:

  - BangPatterns
  - DeriveDataTypeable
  - DeriveGeneric
  - FlexibleContexts
  - FlexibleInstances
  - FunctionalDependencies
  - GeneralizedNewtypeDeriving
  - MultiParamTypeClasses
  - MultiWayIf
  - NamedFieldPuns
  - OverloadedStrings
  - PatternGuards
  - RecordWildCards
  - StandaloneDeriving
  - TupleSections
  - TypeApplications
-}
module Blockchain.Fae.Internal.TX where

import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.Monad 
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Generics

import Language.Haskell.Interpreter as Int 
import Language.Haskell.Interpreter.Unsafe as Int 

import System.FilePath

-- * Types

-- | This is the data type that identifies a transaction in a block; the
-- actual transaction function is only known at the time of interpretation.
data TX =
  TX
  {
    txID :: TransactionID,
    inputs :: Inputs,
    pubKeys :: Signers,
    fallback :: [String]
  }
  deriving (Generic)

newtype UnquotedString = UnquotedString String

-- | This builds on 'FaeStorage' because the interpreter has to access the
-- storage as part of 'runTransaction'.
type FaeInterpret = InterpreterT FaeStorage

{- Instances -}

-- | Default instance
instance Serialize TX
-- | Default instance
instance Digestible TX

instance Show UnquotedString where
  show (UnquotedString s) = s

-- * Functions

-- | Interprets a transaction, looking it up as a module named after its
-- transaction ID; the first argument is whether or not the transaction
-- gets a reward.  We set up the module search path carefully so that this
-- transaction can effectively import both its own other modules, and those
-- of other transactions.  The first transaction is very slow because the
-- interpreter has to initialize, but subsequent ones are acceptably fast.
interpretTX :: Bool -> TX -> FaeInterpret ()
interpretTX isReward TX{..} = handle fixGHCErrors $ do
  Int.set [searchPath := thisTXPath]
  loadModules [txSrc]
  setImportsQ [(txSrc, Just txSrc), ("Blockchain.Fae.Internal", Nothing)]
  run <- interpret runString infer
  lift $ run inputs txID pubKeys isReward 
  where
    runString = intercalate " "
      [
        "runTransaction",
        qualified "body", 
        show $ map (UnquotedString . qualified) fallback
      ]
    fixGHCErrors (WontCompile []) = error "Compilation error"
    fixGHCErrors (WontCompile (ghcE : _)) = error $ errMsg ghcE
    fixGHCErrors (UnknownError e) = error e
    fixGHCErrors (NotAllowed e) = error e
    fixGHCErrors (GhcException e) = error e
    txSrc = "Blockchain.Fae.Transactions.TX" ++ show txID
    thisTXPath = 
      [
        ".",
        "Blockchain" </> "Fae" </> "Transactions" </> 
          ("TX" ++ show txID) </> "private"
      ]
    qualified varName = txSrc ++ "." ++ varName

-- | Runs the interpreter.
runFaeInterpret :: FaeInterpret a -> IO a
runFaeInterpret x = 
  fmap (either throw id) $
  flip evalStateT (Storage Map.empty) $
  getFaeStorage $
  runInterpreter $ do
    Int.set [languageExtensions := languageExts]
    mapM_ Int.unsafeSetGhcOption $
      "-fpackage-trust" :
-- For some reason, this makes the interpreter hang.  Unfortunate, as it
-- rather weakens the trust situation not to have it.
--      "-distrust-all" :
      map ("-trust " ++) trustedPackages
    x

  where
    languageExts =
      [
        BangPatterns,
        DeriveDataTypeable,
        DeriveGeneric,
        FlexibleContexts,
        FlexibleInstances,
        FunctionalDependencies,
        MultiParamTypeClasses,
        MultiWayIf,
        NamedFieldPuns,
        OverloadedStrings,
        PatternGuards,
        RecordWildCards,
        Safe,
        StandaloneDeriving,
        TupleSections,
        TypeApplications
      ]
    trustedPackages =
      [
        "array",
        "base",
        -- not binary,
        "bytestring",
        "containers",
        "fae",
        "filepath",
        "transformers",
        "pretty"
      ]


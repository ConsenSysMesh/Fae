{- |
Module: Blockchain.Fae.Internal.TX
Description: Transaction interpreter
Copyright: (c) Ryan Reich, 2017-2018
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

import Control.DeepSeq

import Control.Monad 
import Control.Monad.State
import Control.Monad.Trans

import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Generics

import Language.Haskell.Interpreter hiding (set,get)
import qualified Language.Haskell.Interpreter as Int (set,get)
import Language.Haskell.Interpreter.Unsafe as Int 

import System.Environment
import System.FilePath

-- * Types

-- | This is the data type that identifies a transaction in a block; the
-- actual transaction function is only known at the time of interpretation.
data TX =
  TX
  {
    isReward :: Bool,
    txID :: TransactionID,
    inputs :: Inputs,
    pubKeys :: Signers,
    fallback :: [String]
  }
  deriving (Generic)

-- | Helpful for printing strings without the surrounding quotes.
newtype UnquotedString = UnquotedString String

-- | Monad for interpreting Fae transactions
type FaeInterpretT m = InterpreterT (FaeStorageT m)

{- Instances -}

-- | Default instance
instance Serialize TX
-- | Default instance
instance Digestible TX
-- | Default instance
instance NFData TX

-- | Prints a string without the quotes
instance Show UnquotedString where
  show (UnquotedString s) = s

-- | -
instance (Monad m) => MonadState Storage (FaeInterpretT m) where
  state = lift . state
  put = lift . put
  get = lift get

-- * Functions

-- | The transaction ID of the "genesis transaction"
nullID :: TransactionID
nullID = ShortContractID $ digest ()

-- | Interprets a transaction, looking it up as a module named after its
-- transaction ID; the first argument is whether or not the transaction
-- gets a reward.  We set up the module search path carefully so that this
-- transaction can effectively import both its own other modules, and those
-- of other transactions.  Now that we dynamically link @faeServer@, the
-- load-up time for the first transaction is pretty short; subsequent
-- transactions are faster still.
interpretTX :: (MonadMask m, MonadIO m) => TX -> FaeInterpretT m ()
interpretTX TX{..} = handle fixGHCErrors $ do
  Int.set [searchPath := thisTXPath]
  loadModules [txSrc]
  setImportsQ [(txSrc, Just txSrc), ("Blockchain.Fae.Internal", Nothing)]
  run <- interpret runString infer
  liftFaeStorage $ run inputs txID pubKeys isReward 
  where
    liftFaeStorage = lift . mapStateT (return . runIdentity) . getFaeStorage
    runString = unwords
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
    txModName = "TX" ++ show txID
    txSrc = "Blockchain.Fae.Transactions." ++ txModName
    thisTXPath = 
      [
        ".",
        "Blockchain" </> "Fae" </> "Transactions" </> txModName </> "private"
      ]
    qualified varName = txSrc ++ "." ++ varName

-- | Runs the interpreter.
runFaeInterpret :: (MonadMask m, MonadIO m) => FaeInterpretT m a -> m a
runFaeInterpret x = do
  ghcLibdirM <- liftIO $ lookupEnv "GHC_LIBDIR"
  fmap (either throw id) $
    flip evalStateT (Storage Map.empty) $ 
      case ghcLibdirM of
        Nothing -> unsafeRunInterpreterWithArgs args x
        Just libdir -> unsafeRunInterpreterWithArgsLibdir args libdir x
  
  where
    args = 
      map ("-X" ++) languageExts ++
      -- For some reason, this makes the interpreter hang.  Unfortunate, as it
      -- rather weakens the trust situation not to have it.
      --      "-distrust-all" :
      "-fpackage-trust" : map ("-trust " ++) trustedPackages
    languageExts = map show
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


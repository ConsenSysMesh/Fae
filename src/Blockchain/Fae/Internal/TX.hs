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
  - TypeFamilies
-}
module Blockchain.Fae.Internal.TX where

import Blockchain.Fae.Internal.Contract
import Blockchain.Fae.Internal.Crypto
import Blockchain.Fae.Internal.Exceptions
import Blockchain.Fae.Internal.IDs
import Blockchain.Fae.Internal.Storage
import Blockchain.Fae.Internal.Transaction

import Control.DeepSeq

import Control.Monad.State

import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Proxy
import Data.Typeable

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
-- | Helpful for eliminating a 'Data.ByteString' dependency in the
-- 'interpretImportedValue' interpreter.
newtype WrappedByteString = 
  WrappedByteString { getWrappedByteString :: ByteString }

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
  get = lift  get

-- * Functions

-- | Interprets a transaction, looking it up as a module named after its
-- transaction ID.  We set up the module search path carefully so that this
-- transaction can effectively import both its own other modules, and those
-- of other transactions.  Now that we dynamically link @faeServer@, the
-- load-up time for the first transaction is pretty short; subsequent
-- transactions are faster still.
interpretTX :: 
  (Typeable m, MonadMask m, MonadIO m) => 
  TX -> FaeInterpretT m ()
interpretTX TX{..} = do
  faeInterpret [txModule] runString $ \f -> f inputs txID pubKeys isReward
  where
    txModule = mkTXModuleName txID
    -- Contrary to comments in the @hint@ documentation, the directory
    -- @"."@ is /not/ always included in the search path.
    runString = unwords
      [
        "runTransaction",
        "body", 
        show $ UnquotedString <$> fallback
      ]

-- | This has to be interpreted because, even though all the information is
-- known /to the sender/ of the value to be imported, it can only be
-- transmitted textually, and therefore has to be re-parsed into a valid
-- type.  The modules necessary for expressing this type are carried in
-- 'neededModules'; they must be present for the recipient, but the
-- corresponding transactions, if any, need not have been fully evaluated.
interpretImportedValue :: 
  (Typeable m, MonadMask m, MonadIO m) => ExportData -> FaeInterpretT m ()
interpretImportedValue ExportData{..} = 
  faeInterpret neededModules runString $ 
    \f -> f (WrappedByteString exportedValue) exportedCID exportStatus
  where
    runString = unwords
      [
        "addImportedValue",
        ".",
        "importValueThrow @(ValType (" ++ exportNameType ++ "))"
      ]

-- | A top-level function (so that it can be in scope in the interpreter of
-- 'interpretImportedValue') to handle failed imports.
importValueThrow :: 
  forall a m.
  (Exportable a, MonadState Escrows m) => WrappedByteString -> m a
importValueThrow (WrappedByteString bs) = 
  fromMaybe (throw $ CantImport bs rep) <$> liftEscrowState (importValue bs)
  where rep = typeRep $ Proxy @a

-- | The inner interpretation function that appropriately loads modules,
-- sets imports, and uses the result both for 'interpretTX' and
-- 'interpretImportedValue'.
faeInterpret :: 
  (Typeable m, MonadMask m, MonadIO m, Typeable a) => 
  [String] -> 
  String ->
  (a -> FaeStorage b) ->
  FaeInterpretT m b
faeInterpret moduleNames runString apply = handle fixGHCErrors $ do
  -- We don't have to, and in fact must not, call `loadModules` on
  -- a package module that isn't to be interpreted.
  loadModules $ filter isTXModule moduleNames 
  setImports imports
  act <- interpret runString infer
  liftFaeStorage $ apply act
  where
    imports =
      "Blockchain.Fae.Internal" :
      "Prelude" :
      filter (not . isInternalModule) moduleNames
    isTXModule = isPrefixOf "Blockchain.Fae.Transactions.TX"
    isInternalModule = isPrefixOf "Blockchain.Fae.Internal"

    fixGHCErrors (WontCompile []) = error "Compilation error"
    fixGHCErrors (WontCompile (ghcE : _)) = error $ errMsg ghcE
    fixGHCErrors (UnknownError e) = error e
    fixGHCErrors (NotAllowed e) = error e
    fixGHCErrors (GhcException e) = error e

    liftFaeStorage = lift . mapStateT (return . runIdentity) . getFaeStorage

-- | Runs the interpreter.
runFaeInterpret :: (MonadMask m, MonadIO m) => FaeInterpretT m a -> m a
runFaeInterpret x = do
  ghcLibdirM <- liftIO $ lookupEnv "GHC_LIBDIR"
  fmap (either throw id) . runStorageT . run ghcLibdirM args $ Int.set opts >> x
  
  where
    run = maybe 
      unsafeRunInterpreterWithArgs 
      (flip unsafeRunInterpreterWithArgsLibdir)
    args = 
      -- We error on orphan instances because the import/export
      -- capability relies on the 'ContractName' instance being in scope
      -- whenever the name itself is.
      "-Werror=orphans" :
      -- For some reason, this makes the interpreter hang.  Unfortunate, as it
      -- rather weakens the trust situation not to have it.
      --      "-distrust-all" :
      "-fpackage-trust" : map ("-trust " ++) trustedPackages
    opts = 
      [
        installedModulesInScope := False, 
        languageExtensions := 
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
            TypeApplications,
            TypeFamilies
          ]
      ]
    trustedPackages =
      [
        "array",
        "base", 
        -- "binary", 
        "bytestring",
        -- "cereal",
        "containers",
        "fae",
        "filepath",
        "transformers",
        "pretty"
      ]

-- | Identifierizes a transaction ID.  On the off chance that an identifier
-- (in some context) cannot begin with a number, we add an alphabetic
-- prefix.
mkTXIDName :: TransactionID -> String
mkTXIDName = ("TX" ++) . show

-- | Defines the Fae runtime module hierarchy.
mkTXPathParts :: TransactionID -> [String]
mkTXPathParts txID = ["Blockchain", "Fae", "Transactions", mkTXIDName txID]

-- | Collapses the transaction path parts into a dot-separated hierarchical
-- module name.
mkTXModuleName :: TransactionID -> String
mkTXModuleName = intercalate "." . mkTXPathParts


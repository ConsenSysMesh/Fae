{- |
Module: FaeServer.Args
Description: Handles command-line arguments for faeServer
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This very simple, ad-hoc command line parser is just explicit
pattern-matching the flag strings; fortunately, we do not accept very
complex flags, so this is appropriate and understandable.
-}

{-# LANGUAGE TemplateHaskell #-}
module FaeServer.Args where

import Common.Lens
import Data.List
import Data.Maybe
import Text.Read

-- | faeServer is either actually running a server of some kind, or
-- printing the help text.
data Args =
  ArgsServer ServerArgs |
  -- | The list contains unrecognized arguments.
  ArgsUsage [String]

-- | This collection of data is passed around and various parts used all
-- over, though not all in any one place.  This kind of suggests that maybe
-- just the one constructor is not appropriate, but it is not yet worth
-- changing.
data ServerArgs =
  ServerArgs
  {
    serverMode :: ServerMode,
    newSession :: Bool,
    evalTimeout :: Int, -- ^ microseconds, 0 is disabled
    faePort :: Int,
    importExportPort :: Int,
    faethHostname :: String,
    faethPort :: Int
  }

-- | Either accepts transactions over HTTP, or from Parity via JSON-RPC.
data ServerMode = FaeMode | FaethMode
 
-- | This uses one of the several synonyms for @localhost@, chosen to
-- prevent Parity from blocking the connection as unsafe.
defaultFaethHost :: String
defaultFaethHost = "127.0.0.1"

-- | This is not our choice; Parity defaults to 8546.
defaultFaethPort :: Int
defaultFaethPort = 8546  

makeLenses ''Args
makePrisms ''Args
makeLenses ''ServerArgs

-- | Fills in the structure by folding the argument list.  Two comments on
-- the defaults: first, the 'newSession' field defaults to 'False' now,
-- whereas it used to be 'True', which will wipe out unsuspecting users'
-- histories; and second, the default port of 27182 is chosen to be the
-- initial decimal digits of the number e.
parseArgs :: [String] -> Args
parseArgs = foldl addArg $
  ArgsServer ServerArgs
  {
    serverMode = FaeMode,
    newSession = False,
    evalTimeout = 1000,
    faePort = 27182,
    importExportPort = 27183,
    faethHostname = defaultFaethHost,
    faethPort = defaultFaethPort
  }

-- | Adds to 'ArgsServer' until a bad argument occurs, then starts
-- accumulating those in 'ArgsUsage'.
addArg :: Args -> String -> Args
addArg args x = getArgAction x & case args of
  ArgsServer{} -> maybe (ArgsUsage [x]) ($ args)
  (ArgsUsage xs) -> maybe (ArgsUsage $ x : xs) (const args)
    
-- | There aren't really any conflicts between flags, so this just sets the
-- appropriate fields for each one.
getArgAction :: String -> Maybe (Args -> Args)
getArgAction = \case 
  x | x == "--faeth" || x == "--faeth-mode" -> Just setFaeth
    | ("--faeth-hostname", '=' : hostnameArgument) <- break (== '=') x ->
      Just $ 
       (_ArgsServer . _faethHostname .~ hostnameArgument) .
       setFaeth
    | ("--faeth-port", '=' : portArgument) <- break (== '=') x ->
      let err = error $ "Could not read port argument: " ++ portArgument in  
      Just $ 
        (_ArgsServer . _faethPort .~ readErr err portArgument) .
        setFaeth
    | ("--fae-port", '=' : faePortArg) <- break (== '=') x ->
      let err = error $ "Could not read port argument: " ++ faePortArg in  
      Just $ _ArgsServer . _faePort .~ readErr err faePortArg
    | ("--import-export-port", '=' : importExportPortArg) <- break (== '=') x ->
      let err=error $ "Could not read port argument: " ++ importExportPortArg in  
      Just $ _ArgsServer . _importExportPort .~ readErr err importExportPortArg
    | ("--eval-timeout", '=' : evalTimeoutArg) <- break (== '=') x ->
      let err = error $ "Could not read timeout argument: " ++ evalTimeoutArg in
      Just $ _ArgsServer . _evalTimeout .~ readErr err evalTimeoutArg
  "--normal-mode" -> Just $ _ArgsServer . _serverMode .~ FaeMode
  "--resume-session" -> Just $ _ArgsServer . _newSession .~ False
  "--new-session" -> Just $ _ArgsServer . _newSession .~ True
  "--help" -> Just $ const $ ArgsUsage []
  _ -> Nothing

  where 
    readErr err = fromMaybe err . readMaybe
    setFaeth = _ArgsServer . _serverMode .~ FaethMode

{-# LANGUAGE TemplateHaskell #-}
module FaeServer.Args where

import Common.Lens
import Data.List
import Data.Maybe
import Text.Read

data Args =
  ArgsServer ServerArgs |
  ArgsUsage [String]

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

data ServerMode = FaeMode | FaethMode
 
defaultFaethHost :: String
defaultFaethHost = "127.0.0.1"

defaultFaethPort :: Int
defaultFaethPort = 8546  

makeLenses ''Args
makePrisms ''Args
makeLenses ''ServerArgs

parseArgs :: [String] -> Args
parseArgs = foldl addArg $
  ArgsServer ServerArgs
  {
    serverMode = FaeMode,
    newSession = True,
    evalTimeout = 1000,
    faePort = 27182,
    importExportPort = 27183,
    faethHostname = defaultFaethHost,
    faethPort = defaultFaethPort
  }

addArg :: Args -> String -> Args
addArg args x = getArgAction x & case args of
  ArgsServer{} -> maybe (ArgsUsage [x]) ($ args)
  (ArgsUsage xs) -> maybe (ArgsUsage $ x : xs) (const args)
    
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

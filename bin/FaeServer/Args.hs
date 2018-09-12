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
    faePort :: Int,
    importExportPort :: Int,
    postTXPort :: Int,
    faethHostname :: String,
    faethPort :: Int
  }

data ServerMode = FaeMode | FaethMode | PostTXMode
 
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
    faePort = 27182,
    importExportPort = 27183,
    postTXPort = 27184,
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
      Just $ _ArgsServer . _faePort .~ read faePortArg
    | ("--import-export-port", '=' : importExportPortArg) <- break (== '=') x ->
      Just $ _ArgsServer . _importExportPort .~ read importExportPortArg
    | ("--posttx-port", '=' : postTXPortArg) <- break (== '=') x ->
              Just $ _ArgsServer . _postTXPort .~ read postTXPortArg
  "--normal-mode" -> Just $ _ArgsServer . _serverMode .~ FaeMode
  "--posttx-mode" -> Just $ _ArgsServer . _serverMode .~ PostTXMode
  "--resume-session" -> Just $ _ArgsServer . _newSession .~ False
  "--new-session" -> Just $ _ArgsServer . _newSession .~ True
  "--help" -> Just $ const $ ArgsUsage []
  _ -> Nothing

  where 
    readErr err = fromMaybe err . readMaybe
    setFaeth = _ArgsServer . _serverMode .~ FaethMode

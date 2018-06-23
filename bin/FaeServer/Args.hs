{-# LANGUAGE TemplateHaskell #-}
module FaeServer.Args where

import Common.Lens

data Args =
  ArgsServer
  {
    serverMode :: ServerMode,
    faePort :: Int,
    importExportPort :: Int,
    flags :: Flags
  } |
  ArgsUsage [String]

data ServerMode = FaeMode | FaethMode

data Flags = 
  Flags
  {
    newSession :: Bool
  }

makeLenses ''Args
makeLenses ''Flags

parseArgs :: [String] -> Args
parseArgs = foldl addArg
  ArgsServer
  {
    serverMode = FaeMode,
    faePort = 27182,
    importExportPort = 27183,
    flags = Flags
      {
        newSession = True
      }
  }

addArg :: Args -> String -> Args
addArg args x = getArgAction x & case args of
  ArgsServer{} -> maybe (ArgsUsage [x]) ($ args)
  (ArgsUsage xs) -> maybe (ArgsUsage $ x : xs) (const args)
    
getArgAction :: String -> Maybe (Args -> Args)
getArgAction = \case 
  x | x == "--faeth" || x == "--faeth-mode" -> Just $ _serverMode .~ FaethMode
    | ("--fae-port", '=' : faePortArg) <- break (== '=') x ->
      Just $ _faePort .~ read faePortArg
    | ("--import-export-port", '=' : importExportPortArg) <- break (== '=') x ->
      Just $ _importExportPort .~ read importExportPortArg
  "--normal-mode" -> Just $ _serverMode .~ FaeMode
  "--resume-session" -> Just $ _flags . _newSession .~ False
  "--new-session" -> Just $ _flags . _newSession .~ True
  "--help" -> Just $ const $ ArgsUsage []
  _ -> Nothing


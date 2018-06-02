{-# LANGUAGE TemplateHaskell #-}
module FaeServer.Args where

import Common.Lens

data Args =
  ArgsServer
  {
    serverMode :: ServerMode,
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
  "--normal-mode" -> Just $ _serverMode .~ FaeMode
  "--resume-session" -> Just $ _flags . _newSession .~ False
  "--new-session" -> Just $ _flags . _newSession .~ True
  "--help" -> Just $ const $ ArgsUsage []
  _ -> Nothing


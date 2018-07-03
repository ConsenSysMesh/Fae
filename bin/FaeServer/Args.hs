{-# LANGUAGE TemplateHaskell #-}
module FaeServer.Args where

import Common.Lens
import Data.List
import Data.Maybe

data Args =
  ArgsServer
  {
    serverMode :: ServerMode,
    flags :: Flags
  } |
  ArgsUsage [String]

data ServerMode = FaeMode | FaethMode

data Flags = Flags
  { newSession :: Bool,
    hostname :: String,
    port :: Int
  } deriving Show
  
defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: Int
defaultPort = 8546  

makeLenses ''Args
makeLenses ''Flags

parseArgs :: [String] -> Args
parseArgs = foldl addArg
  ArgsServer
  {
    serverMode = FaeMode,
    flags = Flags
    
    { 
      newSession = True,
      hostname   = defaultHost,
      port       = defaultPort
    }
  }

addArg :: Args -> String ->  Args
addArg args x = getArgAction x & case args of
  ArgsServer{} -> maybe (ArgsUsage [x]) ($ args)
  (ArgsUsage xs) -> maybe (ArgsUsage $ x : xs) (const args)
   
   
getArgAction :: String -> Maybe (Args -> Args)
getArgAction = \case 
  x | x == "--faeth" || x == "--faeth-mode" -> Just $ _serverMode .~ FaethMode
  x | ("--hostname", '=' : hostnameArgument) <- break (== '=') x
    -> Just $ _flags . _hostname .~ hostnameArgument
  x | ("--port", '=' : portArgument) <- break (== '=') x
      -> Just $ _flags . _port .~ read portArgument
  "--normal-mode" -> Just $ _serverMode .~ FaeMode
  "--resume-session" -> Just $ _flags . _newSession .~ False
  "--new-session" -> Just $ _flags . _newSession .~ True
  "--help" -> Just $ const $ ArgsUsage []
  _ -> Nothing



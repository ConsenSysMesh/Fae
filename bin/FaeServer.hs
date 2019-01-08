{- |
Module: FaeServer
Description: Simple HTTP-based server for Fae transactions
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This program, which rises to slightly above the level of a utility, is
a complete Fae node, excepting the part of that job that involves the
blockchain.  It exposes its functionality via a regular HTTP
query-parameter interface (it would be misleading to call it a REST
protocol) and returns the results, mostly, as JSON, both of which are
hopefully standard enough to be interoperable with other things.
-}

import Control.Concurrent.Lifted
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Proxy

import FaeServer.App
import FaeServer.Args
import FaeServer.Concurrency
import FaeServer.Fae
import FaeServer.Faeth
import FaeServer.Git

import System.Directory
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do
  userHome <- getHomeDirectory
  faeHome <- fromMaybe (userHome </> "fae") <$> lookupEnv "FAE_HOME"
  createDirectoryIfMissing True faeHome
  setCurrentDirectory faeHome
  setEnv "GIT_DIR" "git"

  tID <- myThreadId
  txQueue <- atomically newTQueue
  args <- parseArgs <$> getArgs

  flip runReaderT txQueue $ case args of
    ArgsServer args@ServerArgs{..} -> do
      let portDir = "port-" ++ show faePort
      liftIO $ do
        createDirectoryIfMissing False portDir
        setCurrentDirectory portDir
      void $ fork $ runFae tID args
      void $ fork $ runServer importExportPort importExportApp queueTXExecData
      case serverMode of
        FaeMode -> runServer faePort (serverApp $ Proxy @String) queueTXExecData 
        FaethMode -> runFaeth args tID
    (ArgsUsage xs) -> liftIO $ case xs of
      [] -> do
        usage
        exitSuccess
      xs -> do
        putStrLn $ "Unrecognized option(s): " ++ intercalate ", " xs
        usage
        exitFailure

usage :: IO ()
usage = do
  self <- getProgName
  putStrLn $ unlines
    [
      "Usage: (standalone) " ++ self ++ " options",
      "       (with stack) stack exec " ++ self ++ " -- options",
      "",
      "where the available options are:",
      "  --help                         Print this message",
      "  --normal-mode                  Operate as standalone Fae",
      "  --faeth-mode                   Synonym for --faeth",
      "  --faeth                        Receive transactions via Ethereum from",
      "                                 a Parity client",
      "  --faeth-hostname=string        Connect to Parity at a given hostname",
      "                                 (default: 127.0.0.1)",
      "  --faeth-port=number            Connect to Parity at a given port",
      "                                 (default: 8546)",
      "  --fae-port=number              Listen on a given port for normal-mode",
      "                                 requests (default: 27182)",
      "  --import-export-port=number,   Listen on a given port for import/export",
      "                                 requests (default: 27183)",
      "  --new-session                  Deletes previous transaction history",
      "  --resume-session               Reloads previous transaction history",
      "  --eval-timeout                 Maximum contract or transaction call",
      "                                 duration, in milliseconds",
      "",
      "Later options shadow earlier ones when they have the same domain.",
      "The Fae server listens on port 'fae-port' and accepts import/export data",
      "on 'import-export-port'.",
      "",
      "Recognized environment variables:",
      "  FAE_HOME    Directory where transaction modules and history are stored",
      "",
      "In normal-mode, it receives requests having the following parameters:",
      "  parent: the transaction ID that this one should immediately follow",
      "  view: just recall an existing transaction result",
      "  lazy: don't evaluate the transaction result, just print a generic message",
      "  fake: don't save the transaction in the history, just run it once",
      "  reward: take this to be a 'reward transaction' getting a Reward argument",
      "and expecting, except if 'view = True', the following file parameters",    
      "each one expected to have the filename matching its module name:",
      "  message: the binary serialization of the 'TXMessage'",
      "  body: the module defining 'body :: a -> b -> ... -> FaeTX c'",
      "  other: each time it appears, contains another module that is part of the transaction",
      "In Faeth mode, these are also accepted, but 'fake' is forced to",
      "'True'; live transactions are only accepted through the Ethereum",
      "blockchain.",
      "",
      "The import/export server accepts requests of the form:",
      "  for exporting:",
      "    parent: as for normal mode",
      "    export: a pair (transaction ID, index in its input list) to export",
      "  for importing:",
      "    parent: as for normal mode",
      "    import: a quadruple (cID, status, names, type) where:",
      "      cID = the contract ID to import, with an explicit version",
      "      status = the Status of the call to the cID",
      "      names = list of module names required to express the contract's ContractName",
      "      type = the ContractName itself",
      "    valuePackage: a file containing the binary serialization of the",
      "                  value to be imported"
    ]

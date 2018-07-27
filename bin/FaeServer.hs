{- Server usage:
- Parameter 'key': String (name of a private key, stored on the server)
- Parameter 'main': file (containing 'body :: Transaction a b' definition)
- Parameter 'other': file (containing any module)
- Parameter 'input': (ContractID, String)
-
- If 'main' is not present, then the public key for the 'key' parameter
- will be sent in the response.
-
- The 'main' file and any of the 'other' files can import the 'other' files
- as modules using their local name (say, "module M" is imported as "import
- M" in 'main').  From the 'main' or 'other' file of any other transaction,
- they are imported qualified by "Blockchain.Fae.TX<txID>" (say, "module M"
- in transaction 0a1f34b9 is imported as
- "Blockchain.Fae.Transactions.TX0a1f34b9.M" from another transaction).
- The module declarations are automatically corrected to allow this; you
- upload them with just the local names.
-
- The 'input' parameters are parsed and considered in order as the
- transaction input list.
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
    ArgsServer{..} -> do
      void $ fork $ runFae tID flags
      case serverMode of
        FaeMode -> runFaeServer (Proxy @String) queueTXExecData 
        FaethMode -> runFaeth tID
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
      "  --help            Print this message",
      "  --faeth           Receive transactions via Ethereum from a Parity client",
      "  --faeth-mode      Synonym for --faeth",
      "  --normal-mode     Operate as standalone Fae",
      "  --new-session     Deletes previous transaction history",
      "  --resume-session  Reloads previous transaction history,",
      "",
      "Later options shadow earlier ones when they have the same domain.",
      "",
      "Recognized environment variables:",
      "  FAE_HOME    Directory where transaction modules and history are stored"
    ]

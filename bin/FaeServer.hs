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

import Common.ProtocolT

import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Maybe

import FaeServer.App
import FaeServer.Concurrency
import FaeServer.Fae
import FaeServer.Faeth
import FaeServer.Git

import qualified Network.WebSockets as WS

import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  userHome <- getHomeDirectory
  faeHome <- fromMaybe (userHome </> "fae") <$> lookupEnv "FAE_HOME"
  createDirectoryIfMissing True faeHome
  setCurrentDirectory faeHome
  gitInit

  tID <- myThreadId
  txQueue <- atomically newTQueue
  args <- getArgs

  flip runReaderT txQueue $ do
    void $ fork $ runFae tID
    case args of
      [] -> runFaeServer queueTXExecData 
      ["--faeth"] -> runFaeth tID


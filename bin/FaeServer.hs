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

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)

--------------------------------------------------------------------------------

import Control.Concurrent ( forkIO )
import           Control.Exception
  ( IOException
  , try
  )
import           Data.Default.Class
  ( def
  )
import           Network.Socket             hiding (recv, send)
import qualified Network.TLS                as T
import qualified Network.TLS.Extra          as TE
import           System.Environment
  ( getArgs
  )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
  ( fromMaybe
  , listToMaybe
  )  
--------------------------------------------------------------------------------

tlsPort
  :: IO PortNumber

recv
  :: T.Context
  -> IO (Either IOException BS.ByteString)
  
send
  :: T.Context
  -> [ BS.ByteString ]
  -> IO (Either IOException ())

pong
  :: T.Context
  -> IO ()

spawn
  :: (Socket, SockAddr)
  -> T.Credentials
  -> IO ()

loop
  :: Socket
  -> Either String T.Credential
  -> IO ()

runPostTXServer
  :: IO ()

--------------------------------------------------------------------------------

tlsPort =
  getArgs >>= pure . fromMaybe 8443 . listToMaybe . (map read) 

recv ctx =
  try $ T.recvData ctx

send ctx bs =
  try $ T.sendData ctx $ L8.fromChunks $ bs

pong ctx =
  do
    res <- recv ctx
    case Right "ping" == res of
      False -> T.contextClose ctx
      True  ->
        do
          req <- send ctx $ [ "pong" ]
          case Right () == req of
            False -> T.contextClose ctx
            True  -> pong ctx

spawn (sock, _) creds =
  do
    ctx <- T.contextNew sock $ para creds
    ___ <- T.handshake  ctx
    pong ctx
  where
    para x509 =
      def
      { T.serverWantClientCert = False
      , T.serverShared         = shared
      , T.serverSupported      = supported
      }
      where
        shared =
          def
          { T.sharedCredentials = x509
          }
        supported =
          def
          { T.supportedVersions = [ T.TLS12 ]
          , T.supportedCiphers  = ciphers
          }
        ciphers =
          [ TE.cipher_AES128_SHA1
          , TE.cipher_AES256_SHA1
          , TE.cipher_RC4_128_MD5
          , TE.cipher_RC4_128_SHA1
          ]

loop sock (Right creds) =
  do
    conn <- accept $ sock
    putStrLn $ ("Connected to: " ++) $ show $ snd $ conn
    ____ <- forkIO $ spawn conn $ T.Credentials [creds]
    loop sock $ Right creds
loop ____ (Left msg) =
  putStrLn $ msg 

runPostTXServer =
  do
    port <- tlsPort
    x509 <- T.credentialLoadX509 "/Users/tim.siwula/Documents/Projects/haskell-p2p/tls/localhost.crt" "/Users/tim.siwula/Documents/Projects/haskell-p2p/tls/localhost.key"
    sock <- socket AF_INET Stream 0
    putStrLn $ "Listening on port " ++ show port
    ____ <- setSocketOption sock ReuseAddr 1
    ____ <- bind sock $ SockAddrInet port iNADDR_ANY
    ____ <- listen sock 256
    loop sock x509

--------------------------------------------------------------------------------

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
      --void $ fork $ runPostTXServer $ 27184
            
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
  
  -- loop tls server on port 8843
  runPostTXServer

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
      "  --resume-session               Reloads previous transaction history,",
      "",
      "Later options shadow earlier ones when they have the same domain.",
      "The Fae server listens on port 'fae-port' and accepts import/export data",
      "on 'import-export-port'.",
      "",
      "Recognized environment variables:",
      "  FAE_HOME    Directory where transaction modules and history are stored"
    ]

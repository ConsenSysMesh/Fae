import Blockchain.Fae.Internal hiding ((<.>))

import Control.Concurrent
import Control.Concurrent.STM

import Control.DeepSeq

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.ByteString.Builder

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

import Data.Function
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Serialize as S

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Network.Wai.Parse

import System.Directory
import System.FilePath

import Text.Read (readMaybe)

type TXQueue = TQueue (TX, TMVar String, ThreadId)

main :: IO ()
main = do
  txQueue <- atomically newTQueue
  tID <- myThreadId
  void $ forkIO $ runFae txQueue tID
  runSettings faeSettings $ serverApp txQueue

faeSettings :: Settings
faeSettings = defaultSettings &
  setPort 27182 &
  setOnExceptionResponse exceptionResponseForDebug
  
runFae :: TXQueue -> ThreadId -> IO ()
runFae txQueue mainTID = runFaeInterpret $ forever $ 
  handle (liftIO . throwTo @SomeException mainTID) $ do
    (tx@TX{..}, resultVar, tID) <- liftIO $ atomically $ readTQueue txQueue
    resultE <- try $ do
      interpretTX False tx
      lift $ showTransaction txID
    liftIO $ either 
      (throwTo @SomeException tID)
      (atomically . putTMVar resultVar)
      resultE

serverApp :: TXQueue -> Application
serverApp txQueue request respond = do
  (params, files) <- parseRequestBody lbsBackEnd request
  let
    keyM = lookup "key" params
    key = maybe "key1" C8.unpack keyM
  let 
    inputParams = [ C8.unpack inputBS | ("input", inputBS) <- params ]
    inputs = fromMaybe (error "Couldn't parse inputs") $ 
      mapM readMaybe inputParams

  tx@TX{pubKey, txID} <- nextTX key inputs >>= evaluate . force
  let (mainFileM, modules) = makeFilesMap files txID
  case mainFileM of
    Nothing -> respond $ buildResponse $ key ++ ": " ++ show pubKey
    Just mainFile -> do
      writeModules mainFile modules txID

      tID <- myThreadId
      resultVar <- atomically newEmptyTMVar
      atomically $ writeTQueue txQueue (tx, resultVar, tID)
      result <- atomically $ takeTMVar resultVar

      respond $ buildResponse result

buildResponse :: String -> Response
buildResponse = responseBuilder ok200 headers . stringUtf8 where
  headers = 
    [
      (hContentEncoding, "utf8"),
      (hContentType, "text/plain")
    ]

nextTX :: String -> Inputs -> IO TX
nextTX keyName inputs = do
  keyExists <- doesFileExist keyName
  unless keyExists $ do
    privKey <- newPrivateKey
    B.writeFile keyName $ S.encode (privKey, 0 :: Int) 
  decodeE <- S.decode <$> B.readFile keyName
  let (privKey, nonce :: Int) = either error id decodeE
  B.writeFile keyName $ S.encode (privKey, nonce + 1)
  let 
    Just pubKey = public privKey
    txID = ShortContractID $ digest (pubKey, nonce)
  return TX{..}

makeFilesMap :: 
  [(C8.ByteString, FileInfo LC8.ByteString)] ->
  TransactionID ->
  (Maybe C8.ByteString, Map.Map String C8.ByteString)
makeFilesMap files txID = (txMain, modules) where
  txMain = addHeader txID . LC8.toStrict . fileContent <$> mainFileM
  modules = 
    Map.mapWithKey (fixHeader txID) $ 
    Map.fromList 
      [
        (C8.unpack fileName, LC8.toStrict fileContent) 
          | ("other", FileInfo{..}) <- files
      ]
  mainFileM = lookup "main" files

addHeader :: TransactionID -> C8.ByteString -> C8.ByteString
addHeader txID = C8.append $ C8.pack $
  "module " ++ txModuleName txID ++ " where\n\n" ++
  "import Blockchain.Fae\n\n"

fixHeader :: TransactionID -> String -> C8.ByteString -> C8.ByteString
fixHeader txID fileName = replaceModuleNameWith $ 
  txModuleName txID ++ "." ++ takeBaseName fileName

replaceModuleNameWith :: String -> C8.ByteString -> C8.ByteString
replaceModuleNameWith moduleName contents = 
  pre `C8.append` C8.pack ("module " ++ moduleName ++ " ") `C8.append` post 
  where
    (pre, post0) = C8.breakSubstring "module" contents
    (_, post) = C8.breakSubstring "where" post0
 
writeModules :: 
  C8.ByteString -> Map.Map String C8.ByteString -> TransactionID -> IO ()
writeModules mainFile modules txID = do
  let
    txIDName = "TX" ++ show txID
    txDir = "Blockchain" </> "Fae" </> "Transactions"
    thisTXDir = txDir </> txIDName
    thisTXPrivate = thisTXDir </> "private"
    writeModule fileName fileContents = do
      C8.writeFile (thisTXDir </> fileName) fileContents
      C8.writeFile (thisTXPrivate </> fileName) $ privateModule txID fileName
  createDirectoryIfMissing True thisTXPrivate
  C8.writeFile (txDir </> txIDName <.> "hs") mainFile
  sequence_ $ Map.mapWithKey writeModule modules

privateModule :: TransactionID -> String -> C8.ByteString
privateModule txID fileName = C8.pack $
  "module " ++ moduleName ++ "(module " ++ realModuleName ++ ") where\n\n" ++
  "import " ++ realModuleName ++ "\n" 
  where
    moduleName = takeBaseName fileName
    realModuleName = txModuleName txID ++ "." ++ moduleName

txModuleName :: TransactionID -> String
txModuleName txID = "Blockchain.Fae.Transactions.TX" ++ show txID


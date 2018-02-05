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

import Blockchain.Fae.FrontEnd

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class

import Data.ByteString.Builder

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

import Data.Function
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Serialize as S

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Network.Wai.Parse

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Text.Read (readMaybe)

-- | All the pieces of data that are required to execute a transaction in
-- the dedicated interpreter thread.
data TXExecData =
  TXExecData
  {
    mainFile :: C8.ByteString,
    modules :: Map String C8.ByteString,
    parentM :: Maybe TransactionID,
    fake :: Bool,
    reward :: Bool,
    tx :: TX,
    resultVar :: TMVar String,
    callerTID :: ThreadId
  }

-- | Communications channel with the interpreter thread
type TXQueue = TQueue TXExecData

-- | Tracks all post-transaction states for the purpose of rolling back.
data TXHistory = 
  TXHistory
  {
    txStorageAndCounts :: Map TransactionID (Storage, Integer),
    bestTXID :: TransactionID,
    bestTXCount :: Integer
  }

-- | Monad for tracking history
type FaeInterpretWithHistory = StateT TXHistory FaeInterpret

main :: IO ()
main = do
  gitInit
  txQueue <- atomically newTQueue
  tID <- myThreadId
  void $ forkIO $ runFae txQueue tID
  runSettings faeSettings $ serverApp txQueue

faeSettings :: Settings
faeSettings = defaultSettings &
  setPort 27182 &
  setOnExceptionResponse exceptionResponseForDebug

gitInit :: IO ()
gitInit = do
  setEnv "GIT_DIR" "./.fae-git"
  runGitWithArgs "init" ["--quiet"]
  runGitWithArgs "commit" ["-q", "--allow-empty", "-m", "Transaction " ++ show nullID]
  runGitWithArgs "tag" [txGitTag nullID]

gitCommit :: TransactionID -> IO ()
gitCommit txID = do
  runGitWithArgs "add" ["Blockchain"]
  runGitWithArgs "commit" ["-q", "-m", "Transaction " ++ show txID]
  runGitWithArgs "tag" [txGitTag txID]

gitReset :: TransactionID -> IO ()
gitReset oldTXID = do
  runGitWithArgs "reset" ["--hard", "-q", txGitTag oldTXID]

gitClean :: IO ()
gitClean = runGitWithArgs "clean" ["-q", "-f", "./Blockchain"]

txGitTag :: TransactionID -> String
txGitTag txID = "TX" ++ show txID

runGitWithArgs :: String -> [String] -> IO ()
runGitWithArgs cmd args = do
  let fullArgs = "--work-tree" : "." : cmd : args
  (exitCode, out, err) <- readProcessWithExitCode "git" fullArgs ""
  case exitCode of
    ExitSuccess -> when (not $ null out) $ putStrLn $ unlines
      [
        "`git " ++ cmd ++ "` was successful with the following output:",
        out
      ]
    ExitFailure n -> do
      putStrLn $ unlines $
        ("`git " ++ cmd ++ "` returned code " ++ show n) :
        if null err then [] else
          [            
            "Error message:",
            err
          ]
      exitFailure
  
runFae :: TXQueue -> ThreadId -> IO ()
runFae txQueue mainTID = reThrow mainTID $ runFaeInterpretWithHistory $ 
  forever $ do
    TXExecData{tx=tx@TX{..}, ..} <- ioAtomically $ readTQueue txQueue
    reThrow callerTID $ handle (liftIO . throwTo @ExitCode mainTID) $ do
      dup <- gets $ Map.member txID . txStorageAndCounts
      when dup $ error $ "Duplicate transaction ID: " ++ show txID

      txCount <- recallHistory parentM
      liftIO $ writeModules mainFile modules txID
      txResult <- lift $ do
        interpretTX reward tx
        lift $ showTransaction txID
      if fake
      then liftIO gitClean
      else updateHistory txID txCount
      ioAtomically $ putTMVar resultVar txResult

  where 
    reThrow :: (MonadIO m, MonadCatch m) => ThreadId -> m () -> m ()
    reThrow tID = handleAll (liftIO . throwTo tID)

    ioAtomically :: (MonadIO m) => STM a -> m a
    ioAtomically = liftIO . atomically

recallHistory :: Maybe TransactionID -> FaeInterpretWithHistory Integer
recallHistory parentM = do
  TXHistory{..} <- get
  let parent = fromMaybe bestTXID parentM
  let err = error $ "No transaction in history with ID: " ++ show parent
  -- Weird construct forces this lookup before git runs
  (s, n) <- return $ Map.findWithDefault err parent txStorageAndCounts
  liftIO $ gitReset parent
  liftFaeStorage $ put s
  return n

updateHistory :: TransactionID -> Integer -> FaeInterpretWithHistory ()
updateHistory txID txCount = do
  TXHistory{..} <- get
  s <- liftFaeStorage get
  let newCount = txCount + 1
  let txStorageAndCounts' = Map.insert txID (s, newCount) txStorageAndCounts
  let (bestTXID', bestTXCount')
        | txCount == bestTXCount = (txID, newCount)
        | otherwise = (bestTXID, bestTXCount)
  liftIO $ gitCommit txID
  put $ TXHistory txStorageAndCounts' bestTXID' bestTXCount'

liftFaeStorage :: FaeStorage a -> FaeInterpretWithHistory a
liftFaeStorage = lift . lift

runFaeInterpretWithHistory :: FaeInterpretWithHistory () -> IO ()
runFaeInterpretWithHistory = runFaeInterpret . flip evalStateT emptyTXHistory where
  emptyTXHistory = 
    TXHistory
    {
      txStorageAndCounts = Map.singleton nullID (Storage Map.empty, 0),
      bestTXID = nullID,
      bestTXCount = 0
    }

nullID :: TransactionID
nullID = ShortContractID $ digest ()

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

serverApp :: TXQueue -> Application
serverApp txQueue request respond = do
  (params, files) <- parseRequestBody lbsBackEnd request
  let 
    getParams = getParameters params
    parentM = getLast Nothing Just $ getParams "parent" 
    fake = getLast False id $ getParams "fake" 
    reward = getLast False id $ getParams "reward"
    keyNames = if null keyNames0 then [("self", "key1")] else keyNames0 where
      keyNames0 = map uncolon $ getParams "key"
      uncolon s = (x, tail y) where (x, y) = break (== ':') s
    inputs = fromMaybe inputsErr $ mapM readMaybe $ getParams "input" where
      inputsErr = error "Couldn't parse inputs"
    fallback = getParams "fallback"

  tx@TX{pubKeys, txID} <- nextTX keyNames inputs fallback >>= evaluate . force
  let (mainFileM, modules) = makeFilesMap files txID
  case mainFileM of
    Nothing -> respond $ buildResponse $ 
      intercalate "\n" $ 
      map (\(x,y) -> x ++ ": " ++ show y) $
      Map.toList $
      getSigners pubKeys
    Just mainFile -> do
      callerTID <- myThreadId
      resultVar <- atomically newEmptyTMVar
      atomically $ writeTQueue txQueue TXExecData{..}
      result <- atomically $ takeTMVar resultVar
      respond $ buildResponse result

getParameters :: [(C8.ByteString, C8.ByteString)] -> C8.ByteString -> [String]
getParameters params paramName = 
  [ C8.unpack s | (pName, s) <- params, pName == paramName]

getLast :: (Read b) => a -> (b -> a) -> [String] -> a
getLast x0 f l = last $ x0 : map (f . read) l

buildResponse :: String -> Response
buildResponse = responseBuilder ok200 headers . stringUtf8 where
  headers = 
    [
      (hContentEncoding, "utf8"),
      (hContentType, "text/plain")
    ]

nextTX :: [(String, String)] -> Inputs -> [String] -> IO TX
nextTX keyNames inputs fallback = do
  signerNonces <- forM keyNames $ \(signer, keyName) -> do
    keyExists <- doesFileExist keyName
    unless keyExists $ do
      privKey <- newPrivateKey
      B.writeFile keyName $ S.encode (privKey, 0 :: Int) 
    decodeE <- S.decode <$> B.readFile keyName
    let (privKey, nonce :: Int) = either error id decodeE
    B.writeFile keyName $ S.encode (privKey, nonce + 1)
    let Just pubKey = public privKey
    return (signer, (pubKey, nonce))
  let 
    pubKeys = Signers $ fmap fst $ Map.fromList signerNonces
    txID = ShortContractID $ digest signerNonces
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
  mainFileM = lookup "body" files

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
 
txModuleName :: TransactionID -> String
txModuleName txID = "Blockchain.Fae.Transactions.TX" ++ show txID


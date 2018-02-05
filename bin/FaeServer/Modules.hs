module FaeServer.Modules where

import Blockchain.Fae.FrontEnd

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

import Data.Map (Map)
import qualified Data.Map as Map

import FaeServer.Git

import Network.Wai.Parse

import System.Directory
import System.FilePath

makeFilesMap :: 
  [(C8.ByteString, FileInfo LC8.ByteString)] ->
  TransactionID ->
  (Maybe C8.ByteString, Map String C8.ByteString)
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

writeModules :: 
  C8.ByteString -> Map String C8.ByteString -> TransactionID -> IO ()
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
txModuleName txID = "Blockchain.Fae.Transactions." ++ txGitTag txID


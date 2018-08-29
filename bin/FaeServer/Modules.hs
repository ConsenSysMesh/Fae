module FaeServer.Modules where

import Blockchain.Fae.FrontEnd

import Control.DeepSeq

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List
import Data.Maybe
import Data.Proxy
import Data.Serialize

import FaeServer.Git

import Network.Wai.Parse

import System.Directory
import System.FilePath

type RequestFiles = [(Module, FileInfo LC8.ByteString)]

makeFilesMap :: 
  (Serialize a) => 
  TXMessage a -> Module -> ModuleMap -> Bool -> Bool -> (TX, Module, ModuleMap)
makeFilesMap txMessage mainFile0 modules0 reward isFake = (tx, mainFile, modules) where
  mainFile = addHeader txID mainFile0
  modules = Map.mapWithKey (fixHeader txID . dropExtension) modules0
  tx@TX{..} = 
    maybe (error "Invalid transaction message") force $
    txMessageToTX reward txMessage isFake

getFile :: RequestFiles -> String -> Module
getFile files name = fromMaybe (error $ "Missing " ++ name) $ 
  getFileMaybe files name

getFileMaybe :: RequestFiles -> String -> Maybe Module
getFileMaybe files = last . (Nothing :) . map (Just . snd) . getFiles files 

getFiles :: RequestFiles -> String -> [(String, Module)]
getFiles files name =
  [
    (C8.unpack fileName, LC8.toStrict fileContent) 
      | (name', FileInfo{..}) <- files, name' == C8.pack name
  ]

writeModules :: Module -> ModuleMap -> TransactionID -> IO ()
writeModules mainFile modules txID = do
  let
    thisTXDir = foldr (</>) "" $ mkTXPathParts txID
    thisTXPrivate = mkTXPrivatePath txID
    writeModule fileName fileContents = do
      C8.writeFile (thisTXDir </> fileName) fileContents
      C8.writeFile (thisTXPrivate </> fileName) $ privateModule txID fileName
  createDirectoryIfMissing True thisTXPrivate
  C8.writeFile (thisTXDir <.> "hs") mainFile
  sequence_ $ Map.mapWithKey writeModule modules

privateModule :: TransactionID -> String -> Module
privateModule txID fileName = moduleHeader moduleName (Just exports) realModuleName
  where
    moduleName = takeBaseName fileName
    exports = ["module " ++ realModuleName]
    realModuleName = qualify txID moduleName

addHeader :: TransactionID -> Module -> Module
addHeader txID = C8.append header where 
  header = moduleHeader (mkTXModuleName txID) Nothing "Blockchain.Fae"

fixHeader :: TransactionID -> String -> Module -> Module
fixHeader txID fileName = replaceModuleNameWith (qualify txID fileName)

moduleHeader :: String -> Maybe [String] -> String -> C8.ByteString
moduleHeader moduleName exportsM importModule = C8.pack $
  "module " ++ moduleName ++
  maybe " " (\exports -> " (" ++ intercalate "," exports ++ ") ") exportsM ++
  "where\n\nimport " ++ importModule ++ "\n\n"

replaceModuleNameWith :: String -> Module -> Module
replaceModuleNameWith moduleName contents = 
  pre `C8.append` C8.pack ("module " ++ moduleName ++ " ") `C8.append` post 
  where
    (pre, post0) = C8.breakSubstring "module" contents
    (_, post) = C8.breakSubstring "where" post0
 
qualify :: TransactionID -> String -> String
qualify txID moduleName = mkTXModuleName txID ++ "." ++ moduleName


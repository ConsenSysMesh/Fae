{- |
Module: FaeServer.Modules
Description: A few functions for getting a transaction's modules
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

In addition to organizing the modules attached to a transaction message,
this module also contains the logic to alter their headers, and create
re-export versions, to match the interpreter's expectations.
-}

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

-- | How files in a request are presented.
type RequestFiles = [(Module, FileInfo LC8.ByteString)]

-- | Extracts the transaction object from the full message and arranges the
-- files into a name-contents mapping.
makeFilesMap :: 
  (Serialize a) => 
  TXMessage a -> Module -> ModuleMap -> Bool -> Bool -> (TX, Module, ModuleMap)
makeFilesMap txMessage mainFile0 modules0 reward isFake = (tx, mainFile, modules) where
  mainFile = addHeader txID mainFile0
  modules = Map.mapWithKey (fixHeader txID . dropExtension) modules0
  tx@TX{..} = 
    maybe (error "Invalid transaction message") force $
    txMessageToTX reward txMessage isFake

-- | Looks up a file or throws an error.
getFile :: RequestFiles -> String -> Module
getFile files name = fromMaybe (error $ "Missing " ++ name) $ 
  getFileMaybe files name

-- | Looks up a file, maybe.  The one selected is the last one of that
-- name.
getFileMaybe :: RequestFiles -> String -> Maybe Module
getFileMaybe files = last . (Nothing :) . map (Just . snd) . getFiles files 

-- | Gets all files with a given name, converting to regular data types.
getFiles :: RequestFiles -> String -> [(String, Module)]
getFiles files name =
  [
    (C8.unpack fileName, LC8.toStrict fileContent) 
      | (name', FileInfo{..}) <- files, name' == C8.pack name
  ]

-- | Places the main module and the others into the required directory
-- structure, along with "private" variants.
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

-- | Creates a dummy module that just re-exports the real one.
privateModule :: TransactionID -> String -> Module
privateModule txID fileName = moduleHeader moduleName (Just exports) realModuleName
  where
    moduleName = takeBaseName fileName
    exports = ["module " ++ realModuleName]
    realModuleName = qualify txID moduleName

-- | Attaches the correct module name to the main module, and sets up the
-- implicit @Blockchain.Fae@ import.
addHeader :: TransactionID -> Module -> Module
addHeader txID = C8.append header where 
  header = moduleHeader (mkTXModuleName txID) Nothing "Blockchain.Fae"

-- | Adjusts the name of one of the imported modules to live under the Fae
-- hierarchy (the original name is available as the private variant).
fixHeader :: TransactionID -> String -> Module -> Module
fixHeader txID fileName = replaceModuleNameWith (qualify txID fileName)

-- | Creates a module header with given name, exports, and import.
moduleHeader :: String -> Maybe [String] -> String -> C8.ByteString
moduleHeader moduleName exportsM importModule = C8.pack $
  "module " ++ moduleName ++
  maybe " " (\exports -> " (" ++ intercalate "," exports ++ ") ") exportsM ++
  "where\n\nimport " ++ importModule ++ "\n\n"

-- | Actually does the module name adjustment.  This does
-- a...suspect...kind of parsing to figure out where the name should be:
-- cut out the chunk of the file from the first "module" to the first
-- subsequent "where".  Assuming there are no weird comments above the
-- module header, and that the exports do not include anything with "where"
-- in the name, this should actually work.  I wish there were an accessible
-- Haskell parser I could use.
replaceModuleNameWith :: String -> Module -> Module
replaceModuleNameWith moduleName contents = 
  pre `C8.append` C8.pack ("module " ++ moduleName ++ " ") `C8.append` post 
  where
    (pre, post0) = C8.breakSubstring "module" contents
    (_, post) = C8.breakSubstring "where" post0
 
-- | Adds "Blockchain.Fae.TX<txID>." in front of the module name.
qualify :: TransactionID -> String -> String
qualify txID moduleName = mkTXModuleName txID ++ "." ++ moduleName


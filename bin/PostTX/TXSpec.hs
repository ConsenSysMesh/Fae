{-# LANGUAGE TemplateHaskell #-}
module PostTX.TXSpec (module PostTX.TXSpec, Module, ModuleMap) where

import Blockchain.Fae.FrontEnd

import Common.Lens hiding ((<.>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S
import Data.Serialize (Serialize)

import Data.Maybe

import PostTX.EnvVars

import System.Directory
import System.Environment
import System.FilePath

import Debug.Trace

-- * Spec types
data TXData =
  TXData
  {
    bodyM :: Maybe String,
    others :: [String],
    fallback :: [String],
    inputs :: [(ContractID, String)],
    keys :: [(String, String)],
    reward :: Bool,
    parent :: Maybe TransactionID
  }

data TXSpec =
  TXSpec
  {
    txMessage :: TXMessage,
    mainModule :: (FileName, Module),
    otherModules :: ModuleMap,
    isReward :: Bool,
    parentM :: Maybe TransactionID
  }

type Modules = ModuleMap
type FileName = String
type Keys = Map String PrivateKey
type Identifier = String

-- * Template Haskell
makeLenses ''TXData
makeLenses ''TXSpec

-- * Spec constructor
txDataToSpec :: String -> TXData -> IO TXSpec
txDataToSpec txName TXData{..} = do
  mainModule <- readResolved $ fromMaybe txName bodyM
  otherModules <- Map.fromList <$> mapM readResolved others
  let keys' = if null keys then [("self", "self")] else keys
  privKeys <- Map.fromList <$> mapM (sequence . over _2 getPrivateKey) keys'
  return $ 
    makeTXSpec mainModule otherModules inputs privKeys fallback parent reward

  where 
    readResolved name = do
      rawFile <- readFile fName
      fixedFile <- fmap unlines $ resolveImportVars $ lines rawFile
      return (fName, C8.pack fixedFile)
      where fName = name <.> "hs"

makeTXSpec ::
  (FileName, Module) -> ModuleMap -> Inputs -> Keys -> 
  [Identifier] -> Maybe TransactionID -> Bool -> 
  TXSpec
makeTXSpec mainModule otherModules inputCalls keys fallbackFunctions 
           parentM isReward = 
  TXSpec
  {
    txMessage = addSignatures keys $
      TXMessage
      {
        mainModulePreview = uncurry makePreview mainModule,
        otherModulePreviews = Map.mapWithKey makePreview otherModules,
        signatures = fmap (maybe (error "Bad private key") Left . public) keys,
        salt = "",
        ..
      },
    ..
  }

  where
    makePreview :: FileName -> Module -> ModulePreview
    makePreview fName moduleBS =
      ModulePreview
      {
        moduleDigest = digest $ C8.pack fName `C8.append` moduleBS,
        moduleSize = toInteger $ BS.length moduleBS
      }

    addSignatures :: Keys -> TXMessage -> TXMessage
    addSignatures keys m = Map.foldrWithKey signTXMessage m keys

getPrivateKey :: String -> IO PrivateKey
getPrivateKey name = do
  userHome <- getHomeDirectory
  faeHome <- fromMaybe (userHome </> "fae") <$> lookupEnv "FAE_HOME"
  createDirectoryIfMissing True faeHome
  let fName = faeHome </> name
  keyExists <- doesFileExist fName
  if keyExists
  then either (error $ "Couldn't decode private key: " ++ name) id . 
    S.decode <$> BS.readFile fName
  else do
    privKey <- newPrivateKey
    BS.writeFile fName $ S.encode privKey
    setPermissions fName $ setOwnerReadable True emptyPermissions
    return privKey

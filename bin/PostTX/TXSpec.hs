{-# LANGUAGE TemplateHaskell #-}
module PostTX.TXSpec (module PostTX.TXSpec, Module, ModuleMap) where

import Blockchain.Fae.FrontEnd

import Common.Lens hiding ((<.>))
import Common.ProtocolT

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S
import Data.Serialize (Serialize)

import Data.Maybe

import PostTX.Args

import System.Directory
import System.Environment

-- * Spec types
data TXData a =
  TXData
  {
    dataModules :: a,
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
    specModules :: LoadedModules,
    isReward :: Bool,
    parentM :: Maybe TransactionID
  }

data ParsedModules =
  ParsedModules
  {
    bodyM :: Maybe String,
    others :: [String]
  }

data LoadedModules =
  LoadedModules
  {
    mainModule :: (FileName, Module),
    otherModules :: ModuleMap
  }

type Modules = ModuleMap
type FileName = String
type Keys = Map String PrivateKey
type Identifier = String

-- * Template Haskell
makeLenses ''TXData
makeLenses ''TXSpec
makeLenses ''ParsedModules

-- * Spec constructor
txDataToSpec :: TXData LoadedModules -> FaethArgs -> IO TXSpec
txDataToSpec TXData{..} FaethArgs{..} = do
  let 
    keys' = if null keys then [("self", "self")] else keys
    (signerNames, keyNames) = unzip keys'
  (privKeys, keyNonces) <- unzip <$> mapM getPrivateKey keyNames
  let 
    salt
      | useFaeth = S.encode
          Salt
          {
            faeSalt = show totalNonce, 
            ethFee = faethFee 
          }
      | otherwise = S.encode $ show totalNonce
    totalNonce = sum keyNonces
    privKeyMap = Map.fromList $ zip signerNames privKeys 
  return $ 
    makeTXSpec dataModules inputs privKeyMap fallback parent reward salt

makeTXSpec ::
  LoadedModules -> Inputs -> Keys -> [Identifier] -> 
  Maybe TransactionID -> Bool -> ByteString ->
  TXSpec
makeTXSpec specModules inputCalls keys fallbackFunctions parentM isReward salt = 
  TXSpec
  {
    txMessage = addSignatures keys $
      TXMessage
      {
        mainModulePreview = uncurry makePreview mainModule,
        otherModulePreviews = Map.mapWithKey makePreview otherModules,
        signatures = fmap (maybe (error "Bad private key") Left . public) keys,
        ..
      },
    ..
  }

  where
    LoadedModules{..} = specModules

    makePreview :: FileName -> Module -> ModulePreview
    makePreview fName moduleBS =
      ModulePreview
      {
        moduleDigest = digest $ C8.pack fName `C8.append` moduleBS,
        moduleSize = toInteger $ BS.length moduleBS
      }

    addSignatures :: Keys -> TXMessage -> TXMessage
    addSignatures keys m = Map.foldrWithKey signTXMessage m keys

getPrivateKey :: String -> IO (PrivateKey, Int)
getPrivateKey name = do
  keyExists <- doesFileExist name
  if keyExists
  then do
    p@(key, nonce) <- 
      either (error $ "Couldn't decode private key: " ++ name) id . 
        S.decode <$> BS.readFile name
    BS.writeFile name $ S.encode (key, nonce + 1)
    return p
  else do
    privKey <- newPrivateKey
    let p = (privKey, 0)
    BS.writeFile name $ S.encode p
    return p


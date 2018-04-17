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
import Data.Time.Clock

import PostTX.Args

import System.Directory
import System.Environment

import Text.Read

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
type Keys = Map String (Either PublicKey PrivateKey)
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
  privKeys <- mapM resolveKeyName keyNames
  now <- getCurrentTime
  let 
    salt
      | useFaeth = S.encode
          Salt
          {
            faeSalt = show now, 
            ethFee = HexInteger <$> faethFee 
          }
      | otherwise = S.encode $ show now
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
        signatures = Left . either id (fromMaybe keyErr . public) <$> keys,
        ..
      },
    ..
  }

  where
    LoadedModules{..} = specModules
    keyErr = error "Bad private key"

    makePreview :: FileName -> Module -> ModulePreview
    makePreview fName moduleBS =
      ModulePreview
      {
        moduleDigest = digest $ C8.pack fName `C8.append` moduleBS,
        moduleSize = toInteger $ BS.length moduleBS
      }

    addSignatures :: Keys -> TXMessage -> TXMessage
    addSignatures keys m = Map.foldrWithKey signPrivate m keys

    signPrivate _ (Left k) m = m
    signPrivate s (Right k) m = signTXMessage s k m

resolveKeyName :: String -> IO (Either PublicKey PrivateKey)
resolveKeyName pubKeyS | Just pubKey <- readMaybe pubKeyS = return $ Left pubKey
resolveKeyName name = do
  keyExists <- doesFileExist name
  if keyExists
  then bimap (error $ "Couldn't decode private key: " ++ name) id . 
    S.decode <$> BS.readFile name
  else do
    privKey <- newPrivateKey
    BS.writeFile name $ S.encode privKey
    return $ Right privKey


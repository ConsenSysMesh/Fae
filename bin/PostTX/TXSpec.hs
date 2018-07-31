{-# LANGUAGE TemplateHaskell #-}
module PostTX.TXSpec (module PostTX.TXSpec, Module, ModuleMap) where

import Blockchain.Fae.FrontEnd

import Common.Lens hiding ((<.>))
import Common.ProtocolT

import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Serialize as S
import Data.Serialize (Serialize)

import Data.Maybe
import Data.Time.Clock

import PostTX.Args

import System.Directory

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

data TXSpec a =
  TXSpec
  {
    txMessage :: TXMessage a,
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

class (Monad m, Serialize a) => MakesTXSpec m a where
  txDataToTXSpec :: TXData LoadedModules -> m (TXSpec a)

-- * Template Haskell
makeLenses ''TXData
makeLenses ''TXSpec
makeLenses ''ParsedModules

instance MakesTXSpec IO String where
  txDataToTXSpec = txSpecTimeSalt id

instance MakesTXSpec (ReaderT FaethArgs IO) Salt where
  txDataToTXSpec txData = do
    FaethArgs{..} <- ask
    let 
      makeSalt faeSalt =
        Salt
        {
          ethArgument = maybe BS.empty getHex faethArgument,
          ethFee = HexInteger <$> faethFee,
          ethRecipient = faethRecipient,
          ..
        }
    liftIO $ txSpecTimeSalt makeSalt txData

txSpecTimeSalt :: 
  (Serialize a) => (String -> a) -> TXData LoadedModules -> IO (TXSpec a)
txSpecTimeSalt makeSalt txData = do
  now <- getCurrentTime
  go <- getMakeTXSpec txData
  return $ go $ makeSalt $ show now

getMakeTXSpec :: (Serialize a) => TXData LoadedModules -> IO (a -> TXSpec a)
getMakeTXSpec TXData{..} = do
  let 
    keys' = if null keys then [("self", "self")] else keys
    (signerNames, keyNames) = unzip keys'
  privKeys <- mapM resolveKeyName keyNames
  let privKeyMap = Map.fromList $ zip signerNames privKeys 
  return $ makeTXSpec dataModules inputs privKeyMap fallback parent reward 

makeTXSpec ::
  (Serialize a) => 
  LoadedModules -> Inputs -> Keys -> [Identifier] -> 
  Maybe TransactionID -> Bool -> a ->
  TXSpec a
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

    addSignatures :: (Serialize a) => Keys -> TXMessage a -> TXMessage a
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


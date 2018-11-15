{- |
Module: PostTX.TXSpec
Description: Handler for postTX's Faeth mode
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

A 'TXSpec' contains a complete Fae transaction message, the associated
modules (which are not part of the basic message), and the metadata of
whether this is a reward transaction and what its parent is.  If and when
Fae is managed by a blockchain, both of the latter will be inferred from
the blocks, and the modules will be sent in a separate communication from
the message.
-}
{-# LANGUAGE TemplateHaskell #-}
module PostTX.TXSpec 
  (
    module PostTX.TXSpec, 
    Input(..), Module, ModuleMap, Renames(..), TransactionID, getTXID
  ) where

import Blockchain.Fae.FrontEnd

import Common.Lens
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

import GHC.Generics

import PostTX.Args

import System.Directory

import Text.Read

-- * Spec types

-- | The structure that 'PostTX.Parser' parses a transaction file into.
data TXData =
  TXData
  {
    dataModules :: LoadedModules,
    fallback :: [String],
    materials :: InputMaterials,
    inputs :: [Input],
    keys :: [(String, String)],
    reward :: Bool,
    parent :: Maybe TransactionID
  }

-- | All transaction information appropriately organized.  Recall that the
-- reason the modules are not part of the message is that it is designed to
-- make it possible for a recipient to opt out of incurring significant
-- data costs by discriminating based on file size.  In one-off postTX
-- operation, this dynamic is not present, so the modules are sent
-- alongside the transaction.
data TXSpec a =
  TXSpec
  {
    txMessage :: TXMessage a,
    specModules :: LoadedModules,
    isReward :: Bool,
    parentM :: Maybe TransactionID
  }
  deriving (Generic)

-- | Modules having been read from disk
data LoadedModules =
  LoadedModules
  {
    mainModule :: (FileName, Module),
    otherModules :: Modules
  }
  deriving (Generic)

-- | Less generic.
type FileName = String
-- | Less generic.
type Identifier = String
-- | Convenient.
type Modules = ModuleMap
-- | Definitely convenient.
type Keys = Map String (Either PublicKey PrivateKey)

-- | This is a class in the sense of being actually an overloaded function
-- more than an interface.  Depending on the format of the transaction's
-- "salt", constructing the message may require alternative steps.
-- Presumably the rest of the message is constructed the same way.
class (Monad m, Serialize a) => MakesTXSpec m a where
  txDataToTXSpec :: TXData -> m (TXSpec a)

-- * Template Haskell
makeLenses ''TXData
makeLenses ''TXSpec

-- | -
instance Serialize LoadedModules
-- | -
instance (Serialize a) => Serialize (TXSpec a)

-- | Just uses the time as the salt value.
instance MakesTXSpec IO String where
  txDataToTXSpec = txSpecTimeSalt id

-- | Makes a Faeth salt with Ethereum metadata.
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

-- | Given a partially constructed salt, finishes it off by inserting the
-- current time, then constructs the 'TXSpec' containing it.
txSpecTimeSalt :: 
  (Serialize a) => (String -> a) -> TXData -> IO (TXSpec a)
txSpecTimeSalt makeSalt txData = do
  now <- getCurrentTime
  go <- getMakeTXSpec txData
  return $ go $ makeSalt $ show now

-- | This higher-order function fills in the private keys requested in the
-- 'TXData', then returns a partially constructed 'TXSpec' that expects
-- only the salt, to be provided by 'txSpecTimeSalt'.
getMakeTXSpec :: (Serialize a) => TXData -> IO (a -> TXSpec a)
getMakeTXSpec TXData{..} = do
  let 
    keys' = if null keys then [("self", "self")] else keys
    (signerNames, keyNames) = unzip keys'
  privKeys <- mapM resolveKeyName keyNames
  let keyMap = Map.fromList $ zip signerNames privKeys 
  return $ makeTXSpec dataModules materials inputs keyMap fallback parent reward 

-- | Fills in the 'TXMessage' with the supplied parameters, and also
-- extracts file previews for each module, then signs the whole thing with
-- the provided keys.
makeTXSpec ::
  (Serialize a) => 
  LoadedModules -> InputMaterials -> [Input] -> Keys -> [Identifier] -> 
  Maybe TransactionID -> Bool -> a ->
  TXSpec a
makeTXSpec specModules materialsCalls inputCalls keys 
           fallbackFunctions parentM isReward salt = 
  TXSpec
  {
    txMessage = addSignatures keys
      TXMessage
      {
        mainModulePreview = uncurry makePreview mainModule,
        otherModulePreviews = Map.mapWithKey makePreview otherModules,
        signatures = (,Nothing) . either id (fromMaybe keyErr . public) <$> keys,
        ..
      },
    ..
  }

  where
    LoadedModules{..} = specModules
    keyErr = error "Bad private key"

    -- | Just processes the module as a byte string
    makePreview :: FileName -> Module -> ModulePreview
    makePreview fName moduleBS =
      ModulePreview
      {
        moduleDigest = digest $ C8.pack fName `C8.append` moduleBS,
        moduleSize = toInteger $ BS.length moduleBS
      }

    addSignatures :: (Serialize a) => Keys -> TXMessage a -> TXMessage a
    addSignatures keys m = Map.foldrWithKey addSigner m keys

-- | Signs a 'TXMessage' with a single key in a given role.  This allows
-- for the possibility that the role was /not/ provided with a key, thus
-- producing an incomplete message.  This will be, strictly speaking,
-- invalid, but when sent with @--fake@ is admissible for testing purposes.
addSigner :: 
  (Serialize a) =>
  String -> Either PublicKey PrivateKey -> TXMessage a -> TXMessage a
addSigner _ (Left _) = id
addSigner name (Right privKey) = 
  fromMaybe (error $ "Not a signer role in this transaction: " ++ name) .
  signTXMessage name privKey

-- | If the key "name" parses as a public key, then that is the result and
-- the message will not be signed (by this key).  Otherwise, it is looked
-- up in @faeHome@ as a file containing a private key.
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

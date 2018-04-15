module Common.ProtocolT where

import Blockchain.Fae.FrontEnd

import Common.Lens hiding ((.=))

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

import qualified Network.WebSockets as WS

import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as R

import System.Directory
import System.FilePath

import Text.Read hiding (lift)

data FaethTXData =
  FaethTXData
  {
    faeTX :: TXMessage,
    mainModule :: Module,
    otherModules :: ModuleMap,
    senderEthAccount :: EthAccount,
    faethEthAddress :: EthAddress
  }

data FaeTX = FaeTX TXMessage Module ModuleMap deriving (Generic)

data EthAccount =
  EthAccount
  {
    address :: EthAddress,
    passphrase :: String
  }
  deriving (Generic)

data SendRequest a =
  SendRequest
  {
    sendReqMethod :: String,
    sendReqID :: Int,
    sendReqParams :: a
  }

data ReceiveRequest a =
  ReceiveRequest
  {
    receiveReqMethod :: String,
    receiveReqParams :: a
  }
  
data Response a =
  Response
  {
    respReqID :: Int,
    respData :: Either Error a
  }

data Error =
  Error
  {
    errCode :: Int,
    errMessage :: String,
    errData :: Maybe A.Value
  }

newtype NewAccount = NewAccount String

newtype Hex = Hex { getHex :: ByteString } deriving (Eq, Ord, Serialize)

newtype ProtocolT m a = 
  ProtocolT
  {
    getProtocolT :: StateT Int (ReaderT (WS.Connection, EthAddress) m) a
  }
  deriving 
  (
    Functor, Applicative, Monad, 
    MonadIO, MonadThrow, MonadCatch,
    MonadReader (WS.Connection, EthAddress),
    MonadState Int
  )

class ToRequest a where
  requestMethod :: a -> String

class (MonadIO m) => MonadProtocol m where
  liftProtocolT :: ProtocolT IO a -> m a

class Commutes b m n where
  commute :: m (n a) -> n (m (b a))

type EthAddress = Hex
type EthBlockID = Hex
type EthTXID = Hex

instance Serialize EthAccount
instance Serialize FaeTX

instance Show Error where
  show Error{..} = errMessage ++ maybe "" (\d -> "\n" ++ show d) errData

instance Exception Error

instance Show Hex where
  show = ("0x" ++) . C8.unpack . B16.encode . getHex

instance Read Hex where
  readPrec = Hex . decodeHex . strip0x <$> R.lift rest where 
    strip0x s = fromMaybe s $ L.stripPrefix "0x" s
    rest = RP.many RP.get
    decodeHex = fst . B16.decode . C8.pack

instance ToJSON Hex where
  toJSON = toJSON . show

instance FromJSON Hex where
  parseJSON x = 
    fromMaybe (error "Invalid hex string") . readMaybe <$> parseJSON x

instance ToJSON FaethTXData where
  toJSON FaethTXData{senderEthAccount = EthAccount{..}, ..} =
    toJSON
    [
      A.object
        [
          "from" .= address,
          "to" .= faethEthAddress,
          "data" .= FaeTX faeTX mainModule otherModules
        ],
      toJSON passphrase
    ]

instance ToJSON FaeTX where
  toJSON = toJSON . Hex . S.encode

instance FromJSON FaeTX where
  parseJSON x = 
    either error id . S.decode . getHex <$> parseJSON x

instance (ToJSON a) => ToJSON (SendRequest a) where
  toJSON x = A.object
    [
      "jsonrpc" .= ("2.0" :: Text),
      "method" .= sendReqMethod x,
      "id" .= sendReqID x,
      "params" .= sendReqParams x
    ]

instance (FromJSON a) => FromJSON (ReceiveRequest a) where
  parseJSON = A.withObject "Request" $ \obj -> 
    ReceiveRequest
    <$> obj .: "method"
    <*> obj .: "params"

instance ToJSON NewAccount where
  toJSON (NewAccount password) = toJSON [password]

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = A.withObject "Response" $ \obj -> do
    respReqID <- obj .: "id"
    errorM <- obj .:? "error"
    resultM <- obj .:? "result"
    let
      errorE = Left $ fromMaybe (error "Invalid response") errorM
      respData = maybe errorE Right resultM
    return Response{..}

instance FromJSON Error where
  parseJSON = A.withObject "Error" $ \obj ->
    Error
    <$> obj .: "code"
    <*> obj .: "message"
    <*> obj .:? "data"

instance ToRequest NewAccount where
  requestMethod = const "personal_newAccount"

instance ToRequest FaethTXData where
  requestMethod = const "personal_sendTransaction"

instance {-# OVERLAPPING #-} (MonadIO m) => MonadProtocol (ProtocolT m) where
  liftProtocolT pio =
    ProtocolT $
    StateT $ \s ->
    ReaderT $ \r ->
      liftIO $ runReaderT (runStateT (getProtocolT pio) s) r

instance (MonadProtocol m) => MonadProtocol (ReaderT r m) where
  liftProtocolT = lift . liftProtocolT

instance (MonadProtocol m, Monoid w) => MonadProtocol (WriterT w m) where
  liftProtocolT = lift . liftProtocolT

instance (MonadProtocol m) => MonadProtocol (StateT s m) where
  liftProtocolT = lift . liftProtocolT

instance (MonadProtocol m) => MonadProtocol (ContT r m) where
  liftProtocolT = lift . liftProtocolT

instance (Monad m) => Commutes m (Reader r) (ReaderT r' m) where
  commute x = ReaderT $ return . reader . flip (runReaderT . runReader x)

instance (Monad n) => Commutes n m n where
  commute = return

-- * Wrangling the various reader types

nextID :: Lens' Int Int
nextID = id

liftWS :: (MonadProtocol m) => (WS.Connection -> IO a) -> m a
liftWS f = liftProtocolT (view _1) >>= liftIO . f

askAddress :: (MonadProtocol m) => m EthAddress
askAddress = liftProtocolT $ view _2

splitProtocolT :: 
  ProtocolT m a -> StateT Int (ReaderT EthAddress (ReaderT WS.Connection m)) a
splitProtocolT x = 
  StateT $ \nextID ->
  ReaderT $ \addr ->
  ReaderT $ \conn -> 
    runReaderT (runStateT (getProtocolT x) nextID) (conn, addr)

runProtocolT :: 
  (MonadIO m, Commutes IO (Reader WS.Connection) m) => 
  EthAddress -> ProtocolT m () -> m ()
runProtocolT address x = do
  liftIO $ putStrLn $
    "Connecting to Ethereum client (" ++ host ++ ":" ++ show port ++ ")"
  xWS <- 
    commute $ 
    reader $
    runReaderT $
    flip runReaderT address $
    flip evalStateT 0 $
    splitProtocolT x
  liftIO $ WS.runClient host port "" $ runReader xWS

  where
    host = "localhost"
    port = 8546

sendProtocolT :: (ToJSON a, ToRequest a, MonadProtocol m) => a -> m Int
sendProtocolT sendReqParams = do
  sendReqID <- liftProtocolT $ use nextID
  liftProtocolT $ nextID += 1
  let sendReqMethod = requestMethod sendReqParams
  liftWS $ flip WS.sendTextData $ A.encode SendRequest{..}
  return sendReqID

receiveProtocolT :: (FromJSON a, MonadProtocol m) => Int -> m a
receiveProtocolT reqID = do
  Response{..} <- either error id . A.eitherDecode <$> liftWS WS.receiveData
  return $
    if respReqID == reqID
    then either throw id respData
    else error $ 
      "Expected response with ID " ++ show reqID ++ 
      "; got " ++ show respReqID

receiveRequest :: 
  (FromJSON a, ToRequest a, MonadProtocol m) => String -> m a
receiveRequest method = do
  ReceiveRequest{..} <- either error id . A.eitherDecode <$> liftWS WS.receiveData
  unless (receiveReqMethod == requestMethod receiveReqParams) $ error $ 
    "Expected request with method " ++ receiveReqMethod ++ 
    "; got " ++ requestMethod receiveReqParams
  return receiveReqParams

sendReceiveProtocolT :: 
  (ToJSON a, ToRequest a, FromJSON b, MonadProtocol m) => a -> m b
sendReceiveProtocolT x = do
  reqID <- sendProtocolT x
  receiveProtocolT reqID

readAccount :: (MonadIO m) => String -> m EthAccount
readAccount name = liftIO $ 
  either (error $ "Couldn't decode " ++ name ++ " account file") id .
    S.decode <$> BS.readFile accountPath
  where accountPath = "faeth" </> (name ++ "EthAccount")

newAccount :: (MonadProtocol m) => String -> String -> m EthAccount
newAccount name passphrase = do
  address <- sendReceiveProtocolT (NewAccount passphrase)
  let account = EthAccount{..}
  writeAccount name account
  return account

writeAccount :: (MonadIO m) => String -> EthAccount -> m ()
writeAccount name account = liftIO $ do
  let accountPath = "faeth" </> (name ++ "EthAccount")
  createDirectoryIfMissing True "faeth"
  BS.writeFile accountPath $ S.encode account

nullAddress :: EthAddress
nullAddress = Hex BS.empty

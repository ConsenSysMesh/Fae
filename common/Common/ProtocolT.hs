{- |
Module: Common.ProtocolT
Description: Reusable parts of the Faeth JSON-RPC communication
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

This module defines types and functions that are generally useful both to
the server (@faeServer@) and the client (@postTX@) when communicating with
an Ethereum client via JSON-RPC.  Primary among these is the
request/response/error framework of 'SendRequest', 'ReceiveResponse', and
'Error', together with 'sendProtocolT', 'receiveProtocolT',
'receiveRequest', and 'sendReceiveProtocolT'.  The communication abstraction of the 'ProtocolT' monad is also defined with its initializer 'runProtocolT'.  Finally, the basic Faeth transaction types are defined here. 

-}
{-# LANGUAGE TemplateHaskell #-}
module Common.ProtocolT where

import Blockchain.Fae.FrontEnd

import Codec.Compression.Zlib

import Common.Lens hiding ((.=))

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import GHC.Generics as G

import qualified Network.WebSockets as WS

import Numeric

import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as RP

import Text.Read hiding (lift, get)

-- | A description of Faeth transactions both for sending new ones and for
-- receiving old ones that are to be modified and sent back.  The Ethereum
-- fields that are included here are sort of arbitrary in that they reflect
-- only the intended functionality of Faeth as it currently exists; this
-- does not at all capture the complexity of an Ethereum transaction.
data FaethTXData =
  FaethTXData
  {
    faeTX :: EthArgFaeTX,
    senderEthAccount :: EthAccount,
    faethEthAddress :: EthAddress,
    faethEthValue :: Maybe HexInteger
  }

-- | A package containing the full information of a Fae transaction, for
-- encoding and decoding in the @input@ field of the host Ethereum
-- transaction.
data FaeTX = 
  FaeTX 
  {
    faeTXMessage :: TXMessage Salt, -- ^ Needs to be the first field
    faeMainModule :: Module,
    faeOtherModules :: ModuleMap
  } deriving (Generic)

-- | For defining variant encodings
newtype EthArgFaeTX = EthArgFaeTX { getEthArgFaeTX :: FaeTX }

-- | Faeth's particular format for the Fae "extra data field" @salt@.  In
-- addition to the Ethereum value and recipient demanded, it also
-- reintroduces an unformatted field so that clients can still put
-- distinguishing information to ensure unique transaction IDs.
data Salt = 
  Salt
  {
    ethArgument :: ByteString, -- ^ Needs to be the first field
    ethFee :: Maybe HexInteger,
    ethRecipient :: Maybe EthAddress,
    faeSalt :: String
  }
  deriving (Generic)

-- | A possibly Parity-specific representation of all the information
-- necessary to sign an Ethereum transaction.  Other clients or wallets
-- will, probably, have different access mechanisms than a passphrase.
data EthAccount =
  EthAccount
  {
    address :: EthAddress,
    passphrase :: String
  }
  deriving (Generic)

-- | The top-level protocol format for a JSON-RPC message, having nothing
-- to do with Ethereum in particular.  We ignore the JSON-RPC version,
-- though I guess we should actually just assume that it is always 2.0.
data SendRequest a =
  SendRequest
  {
    sendReqMethod :: String,
    sendReqID :: Int,
    sendReqParams :: a
  }

-- | The format for a received message.  Although we are running an RPC
-- client, not server, we do request new blocks via a subscription, which
-- returns them as requests (presumably because the message originates at
-- the server and is not part of the same exchange that created the
-- subscription).
data ReceiveRequest a =
  ReceiveRequest
  {
    receiveReqMethod :: String,
    receiveReqParams :: a
  }
  
-- | The format for a response from the server.
data Response a =
  Response
  {
    respReqID :: Int,
    respData :: Either Error a
  }

-- | Errors that arise in processing a successfully transmitted JSON-RPC
-- message.
data Error =
  Error
  {
    errCode :: Int,
    errMessage :: String,
    errData :: Maybe A.Value
  }

-- | A semantically meaningful bytestring, intended to be encoded and
-- decoded as hexadecimal strings.
newtype Hex = Hex { getHex :: ByteString } deriving (Eq, Ord, Serialize)

-- | Like 'Hex', but for integers, which are parsed from (big-endian) hex
-- strings.
newtype HexInteger = HexInteger { getHexInteger :: Integer }
  deriving (Serialize, Eq, Ord, Num, Real, Enum, Integral) 

-- | 'ProtocolT' is a monad transformer exposing all the information
-- necessary to run a functioning JSON-RPC client over websockets.  This
-- consists of an open websocket connection and an updating integer, equal
-- to the number of messages that have been sent over this connection (for
-- producing correct request IDs, not that we really care).
newtype ProtocolT m a = 
  ProtocolT
  {
    getProtocolT :: StateT Int (ReaderT WS.Connection m) a
  }
  deriving 
  (
    Functor, Applicative, Monad, 
    MonadIO, MonadThrow, MonadCatch,
    MonadReader WS.Connection,
    MonadState Int
  )

-- | I stole this from the 'json-rpc' package, because I didn't like most
-- of the package but this concept made a lot of sense.  An instance of
-- 'ToRequest' declares the exact @method@ string that is passed when
-- a type representing a request is converted to a JSON-RPC message.
class ToRequest a where
  requestMethod :: a -> String

-- | A further abstraction of 'ProtocolT', this class generalizes 'liftIO'
-- to lifting an entire 'ProtocolT' over 'IO'.
class (MonadIO m, MonadCatch m) => MonadProtocol m where
  liftProtocolT :: ProtocolT IO a -> m a

-- | This class is inspired by the 'monad-control-aligned' package, which
-- is used to generalize types involving, say, 'IO' to general 'MonadIO'
-- instances.  Basically it allows one to obtain a value in the base monad
-- of a stack from within the full stack.
class Commutes b m n where
  commute :: m (n a) -> n (m (b a))

-- | The address field of a JSON-RPC Ethereum transaction is a hex string
-- (of length 64, but we don't care).
type EthAddress = Hex
-- | Ethereum block IDs are given via JSON-RPC as hex strings (of length
-- 64, but we don't care).
type EthBlockID = Hex
-- | Ethereum transaction IDs are given via JSON-RPC as hex strings (of length
-- 64, but we don't care).
type EthTXID = Hex

makeLenses ''FaethTXData
makeLenses ''FaeTX
makeLenses ''Salt
makeLenses ''EthArgFaeTX

-- | Generic instance
instance Serialize EthAccount
-- | Generic instance
instance Serialize FaeTX

-- | Differs from the default encoding of FaeTX in that the length of the
-- Ethereum argument is shifted from the beginning to the end, exposing the
-- argument itself.  This allows an Ethereum contract to use the encoding
-- as its argument, so long as it follows the Ethereum Contract ABI, in
-- which the actual argument is well-typed and (for ambiguously sized
-- types) length-prefixed.  The length suffix can be re-relocated for
-- decoding.
--
-- This code depends strongly on the assumption that the Generic encoding
-- of a product type is just the concatenation of the encodings of its
-- fields, and that a ByteString's encoding is length-prefixed by an 'Int'.
instance Serialize EthArgFaeTX where
  put (EthArgFaeTX faeTX) = 
    let encoding = S.encode faeTX
        ethArg = faeTX ^. _faeTXMessage . _salt . _ethArgument
        dataBS = S.encode ethArg
        compressStrict = LBS.toStrict . compress . LBS.fromStrict
        compressed = maybe (error "FaeTX doesn't start with ethArgument!") 
          compressStrict $ BS.stripPrefix dataBS encoding
        ethArgPrefix = fromMaybe (error "ethArgument doesn't end with the data!") $
          BS.stripSuffix ethArg dataBS
    in S.putByteString $ ethArg <> compressed <> ethArgPrefix

  get = do
    encoding <- S.remaining >>= S.getBytes
    let intLength = BS.length (S.encode (0 :: Int)) -- ^ Kind of a hack
        encLength = BS.length encoding
        (rest, argLengthBS) = BS.splitAt (encLength - intLength) encoding
        argLength = either error id $ S.decode argLengthBS
        (dataBS, compressed) = BS.splitAt argLength rest
        decompressStrict = LBS.toStrict . decompress . LBS.fromStrict
        decompressed = decompressStrict compressed
    faeTX <- either fail return $ S.decode $ argLengthBS <> dataBS <> decompressed
    return $ EthArgFaeTX faeTX

-- | -
instance Serialize Salt

-- | -
instance Show Error where
  show Error{..} = errMessage ++ maybe "" (\d -> "\n" ++ show d) errData

-- | -
instance Exception Error

-- | -
instance Show Hex where
  show = ("0x" ++) . C8.unpack . B16.encode . getHex

-- | -
instance Read Hex where
  readPrec = R.readP_to_Prec $ const $ do
    (orig, (hexBS, rest)) <- RP.gather $ do
      RP.optional $ RP.string "0x"
      B16.decode . C8.pack <$> (RP.get `RP.manyTill` RP.eof)
    if BS.null rest
    then return $ Hex hexBS
    else fail $ "Invalid hex string: " ++ orig

-- | -
instance ToJSON Hex where
  toJSON = toJSON . show

-- | -
instance FromJSON Hex where
  parseJSON = A.withText "Hex" $ either fail return . readEither . T.unpack

-- | -
instance Show HexInteger where
  show = ("0x" ++) . flip showHex ""

-- | -
instance Read HexInteger where
  readPrec = HexInteger <$> R.readS_to_Prec (const readHex)

-- | -
instance ToJSON HexInteger where
  toJSON = toJSON . show

-- | -
instance FromJSON HexInteger where
  parseJSON x = flip (A.withText "HexInteger") x $ \t -> do
    let hexE = T.hexadecimal t
    either (const $ A.typeMismatch "HexInteger" x) (return . fst) hexE

-- | -
instance ToJSON FaethTXData where
  toJSON FaethTXData{senderEthAccount = EthAccount{..}, ..} =
    toJSON
    [
      A.object $
        (maybe id (:) $ ("value" .=) <$> faethEthValue) $
        [
          "from" .= address,
          "to" .= faethEthAddress,
          "data" .= faeTX
        ], 
      toJSON passphrase
    ]

-- | -
instance FromJSON FaethTXData where
  parseJSON = A.withObject "FaethTXData" $ \obj -> do
    faeTX <- obj .: "input"
    faethEthAddress <- obj .: "to"
    faethEthValue <- obj .: "value"
    address <- obj .: "from"
    let
      passphrase = 
        error $ "Unknown passphrase for Ethereum account: " ++ show address
    return
      FaethTXData
      {
        senderEthAccount = EthAccount{..},
        ..
      }

-- | -
instance ToJSON EthArgFaeTX where
  toJSON = toJSON . Hex . S.encode

-- | -
instance FromJSON EthArgFaeTX where
  parseJSON x = either error id . S.decode . getHex <$> parseJSON x

-- | -
instance (ToJSON a) => ToJSON (SendRequest a) where
  toJSON x = A.object
    [
      "jsonrpc" .= ("2.0" :: Text),
      "method" .= sendReqMethod x,
      "id" .= sendReqID x,
      "params" .= sendReqParams x
    ]

-- | -
instance (FromJSON a) => FromJSON (ReceiveRequest a) where
  parseJSON = A.withObject "Request" $ \obj -> 
    ReceiveRequest
    <$> obj .: "method"
    <*> obj .: "params"

-- | -
instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = A.withObject "Response" $ \obj -> do
    respReqID <- obj .: "id"
    errorM <- obj .:? "error"
    resultM <- obj .:? "result"
    let
      errorE = Left $ fromMaybe (error "Invalid response") errorM
      respData = maybe errorE Right resultM
    return Response{..}

-- | -
instance FromJSON Error where
  parseJSON = A.withObject "Error" $ \obj ->
    Error
    <$> obj .: "code"
    <*> obj .: "message"
    <*> obj .:? "data"

-- | -
instance ToRequest FaethTXData where
  requestMethod = const "personal_sendTransaction"

-- | -
instance {-# OVERLAPPING #-} 
  (MonadIO m, MonadCatch m) => MonadProtocol (ProtocolT m) where

  liftProtocolT pio =
    ProtocolT $
    StateT $ \s ->
    ReaderT $ \r ->
      liftIO $ runReaderT (runStateT (getProtocolT pio) s) r

-- | -
instance (MonadProtocol m) => MonadProtocol (ReaderT r m) where
  liftProtocolT = lift . liftProtocolT

-- | -
instance (MonadProtocol m, Monoid w) => MonadProtocol (WriterT w m) where
  liftProtocolT = lift . liftProtocolT

-- | -
instance (MonadProtocol m) => MonadProtocol (StateT s m) where
  liftProtocolT = lift . liftProtocolT

-- | -
instance (Monad m) => Commutes m (Reader r) (ReaderT r' m) where
  commute x = ReaderT $ return . reader . flip (runReaderT . runReader x)

-- | -
instance (Monad n) => Commutes n m n where
  commute = return

-- * Wrangling the various reader types

-- | Manipulates the request ID counter; this is more or less specific to
-- an actual 'ProtocolT' and not just a 'MonadProtocol', though of course
-- there is always 'liftProtocolT' to help with the latter; we can't
-- enforce this convention in this function because the type of a lens is
-- independent of the monad in which it may be used.
nextID :: Lens' Int Int
nextID = id

-- | The @websockets@ library rather annoyingly doesn't use a reader monad
-- for its connection, but this effectively lifts that monad into
-- a 'MonadProtocol'.
liftWS :: (MonadProtocol m) => (WS.Connection -> IO a) -> m a
liftWS f = liftProtocolT ask >>= liftIO . f

-- | Opens a websocket connection with whatever handshake that protocol
-- uses (handled by @websockets@, not by us) and initializes the ID counter
-- to 0.
runProtocolT :: (MonadIO m, Commutes IO (Reader WS.Connection) m) => String -> Int -> ProtocolT m () -> m ()
runProtocolT host port x = do
  liftIO $ putStrLn $
    "Connecting to Ethereum client (" ++ host ++ ":" ++ show port ++ ")\n"
  xWS <- 
    commute $ 
    reader $
    runReaderT $
    flip evalStateT 0 $
    getProtocolT x
  liftIO $ WS.runClient host port "" $ runReader xWS

-- | Sends a JSON-RPC message over a websocket.
sendProtocolT :: (ToJSON a, ToRequest a, MonadProtocol m) => a -> m Int
sendProtocolT sendReqParams = do
  sendReqID <- liftProtocolT $ use nextID
  liftProtocolT $ nextID += 1
  let sendReqMethod = requestMethod sendReqParams
  liftWS $ flip WS.sendTextData $ A.encode SendRequest{..}
  return sendReqID

-- | Receives a JSON-RPC response over a websocket.
receiveProtocolT :: (FromJSON a, MonadProtocol m) => Int -> m a
receiveProtocolT reqID = do
  Response{..} <- either error id . A.eitherDecode <$> liftWS WS.receiveData
  return $
    if respReqID == reqID
    then either throw id respData
    else error $ 
      "Expected response with ID " ++ show reqID ++ 
      "; got " ++ show respReqID

-- | Receives a JSON-RPC request over a websocket (i.e. for a subscription
-- update).
receiveRequest :: 
  (FromJSON a, ToRequest a, MonadProtocol m) => String -> m a
receiveRequest method = do
  ReceiveRequest{..} <- either error id . A.eitherDecode <$> liftWS WS.receiveData
  unless (receiveReqMethod == requestMethod receiveReqParams) $ error $ 
    "Expected request with method " ++ receiveReqMethod ++ 
    "; got " ++ requestMethod receiveReqParams
  return receiveReqParams

-- | A convenient all-in-one communication than handles the ID wrangling
-- and abstracts away the error as an exception.
sendReceiveProtocolT :: 
  (ToJSON a, ToRequest a, FromJSON b, MonadProtocol m) => a -> m b
sendReceiveProtocolT x = handleAll err $ do
  reqID <- sendProtocolT x
  receiveProtocolT reqID
  where err e = error $ "Ethereum client returned an error: " ++ show e


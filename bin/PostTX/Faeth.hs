module PostTX.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Monad.Trans

import Data.Aeson (ToJSON(..))

import PostTX.TXSpec

newtype GetFaethTX = GetFaethTX EthTXID

instance ToJSON GetFaethTX where
  toJSON (GetFaethTX ethTXID) = toJSON [ethTXID]

instance ToRequest GetFaethTX where
  requestMethod _ = "eth_getTransactionByHash"

submitFaeth :: String -> Maybe Integer -> TXSpec -> IO ()
submitFaeth host valM TXSpec{specModules = LoadedModules{..}, ..} = do
  senderEthAccount <- readAccount "sender"
  EthAccount{address = faethEthAddress} <- readAccount "faeth"
  runProtocolT faethEthAddress $ do
    ethTXID <- sendReceiveProtocolT 
      FaethTXData
      {
        faeTX = txMessage, 
        mainModule = snd mainModule, 
        faethEthValue = HexInteger <$> valM,
        ..
      }
    liftIO . putStrLn $ 
      "Ethereum transaction ID: " ++ ethTXID ++
      "\nFae transaction ID: " ++ show (getTXID txMessage)

resubmitFaeth :: String -> [String] -> EthTXID -> IO ()
resubmitFaeth host addSignerNames ethTXID = do
  senderEthAccount <- readAccount "sender"
  EthAccount{address = faethEthAddress} <- readAccount "faeth"
  runProtocolT faethEthAddress $ do
    faethTXData <- sendReceiveProtocolT $ GetFaethTX ethTXID
    addSignerKeys <- liftIO $ mapM resolveKeyName addSignerNames
    let 
      addSigners =
        foldr (.) id $
        zipWith addSigner addSignerNames addSignerKeys
    ethTXID <- sendReceiveProtocolT $
      faethTXData
      & _faeTX %~ addSigners
      & _senderEthAccount .~ senderEthAccount
    liftIO . putStrLn $
      "New Ethereum transaction ID: " ++ ethTXID ++
      "\nfor Fae transaction: " ++ show (getTXID $ faeTX faethTXData)

addSigner :: String -> Either PublicKey PrivateKey -> TXMessage -> TXMessage
addSigner _ (Left _) = id
addSigner name (Right privKey) = signTXMessage name privKey

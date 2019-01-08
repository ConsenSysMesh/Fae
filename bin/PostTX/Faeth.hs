{- |
Module: PostTX.Faeth
Description: Handler for postTX's Faeth mode
Copyright: (c) Ryan Reich, 2017-2018
License: MIT
Maintainer: ryan.reich@gmail.com
Stability: experimental

The two 'submit' functions craft a JSON-RPC message for Parity describing
the transaction to submit or to look up.  All that logic, however, is in
'Common.ProtocolT'; we just fill in some blanks here.
-}
module PostTX.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Monad.Trans

import Data.Aeson (ToJSON(..))
import Data.Maybe

import PostTX.Args
import PostTX.TXSpec

import System.Console.Haskeline

import Text.Read

-- | Since an 'EthTXID' is actually just a byte string, this gives it
-- a distinct identity so that it can have the following instances.
newtype GetFaethTX = GetFaethTX EthTXID

-- | -
instance ToJSON GetFaethTX where
  toJSON (GetFaethTX ethTXID) = toJSON [ethTXID]

-- | -
instance ToRequest GetFaethTX where
  requestMethod _ = "eth_getTransactionByHash"

-- | Sends a new Fae transaction to Parity, wrapped in an Ethereum
-- transaction.  The details of this are in 'Common.ProtocolT' and
-- 'FaeServer.Faeth'.
submitFaeth :: String -> Maybe Integer -> Maybe EthAddress -> TXSpec Salt -> IO ()
submitFaeth host valM faethTo TXSpec{specModules = LoadedModules{..}, ..} = do
  senderEthAccount <- inputAccount
  let (hostname, ':' : port) = break (== ':') host
  let portNum = fromMaybe (error $ "Bad port number: " ++ port) $ readMaybe port
  runProtocolT hostname portNum $ do
    ethTXID <- sendReceiveProtocolT 
      FaethTXData
      {
        faeTX = 
          EthArgFaeTX FaeTX
          {
            faeTXMessage = txMessage,
            faeMainModule = snd mainModule, 
            faeOtherModules = otherModules
          },
        faethEthValue = HexInteger <$> valM,
        faethEthAddress = fromMaybe (address senderEthAccount) faethTo,
        ..
      }
    liftIO . putStrLn $ 
      "Ethereum transaction ID: " ++ ethTXID ++
      "\nFae transaction ID: " ++ show (getTXID txMessage)

-- | Requests a previously entered Faeth transaction from Parity (or
-- rather, requests an Ethereum transaction and tries to extract a Fae
-- transaction from it), then changes various parameters of the Ethereum
-- transaction to match the ones required in the Fae transaction's 'Salt'.
resubmitFaeth :: String -> EthTXID -> FaethArgs -> IO ()
resubmitFaeth host ethTXID FaethArgs{..} = do
  senderEthAccount <- inputAccount
  let (hostname, ':' : port) = break (== ':') host
  let portNum = fromMaybe (error $ "Bad port number: " ++ port) $ readMaybe port
  runProtocolT hostname portNum $ do
    faethTXData <- sendReceiveProtocolT $ GetFaethTX ethTXID
    newKeys <- liftIO $ mapM resolveKeyName newKeyNames
    let 
      addSigners =
        foldr (.) id $
        zipWith addSigner newNames newKeys
      txID = getTXID $ faeTXMessage $ getEthArgFaeTX $ faeTX faethTXData
    ethTXID <- sendReceiveProtocolT $
      faethTXData
      & _faeTX . _getEthArgFaeTX . _faeTXMessage %~ addSigners
      & _senderEthAccount .~ senderEthAccount
      & _faethEthAddress .~ fromMaybe (address senderEthAccount) faethTo
      & _faethEthValue .~ (HexInteger <$> faethValue)
    liftIO . putStrLn $
      "New Ethereum transaction ID: " ++ ethTXID ++
      "\nfor Fae transaction: " ++ show txID

  where (newNames, newKeyNames) = unzip newSigners

-- | Console routine to take Ethereum account information.
inputAccount :: IO EthAccount
inputAccount = runInputT defaultSettings $ 
  EthAccount <$> inputAddress <*> inputPassphrase

-- | Accepts the account address, echoing the input.
inputAddress :: InputT IO EthAddress
inputAddress = do
  addressSM <- getInputLine "Ethereum address: "
  let addressM = addressSM >>= readMaybe
  maybe (error "Bad address") return addressM

-- | Accepts the account password, /not/ echoing it.
inputPassphrase :: InputT IO String
inputPassphrase = do
  passphraseM <- getInputLine "Passphrase: "
  maybe (error "Bad passphrase") return passphraseM

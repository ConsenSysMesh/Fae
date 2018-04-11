module PostTX.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Monad.Trans

import PostTX.TXSpec

submitFaeth :: String -> TXSpec -> IO ()
submitFaeth host TXSpec{specModules = LoadedModules{..}, ..} = do
  senderEthAccount <- readAccount "sender"
  EthAccount{address = faethEthAddress} <- readAccount "faeth"
  runProtocolT faethEthAddress $ do
    respE <- sendReceiveProtocolT 
      FaethTXData
      {
        faeTX = txMessage, 
        mainModule = snd mainModule, 
        ..
      }
    case respE of
      Left e -> error $ "Ethereum client returned an error: " ++ show e
      Right ethTXID -> liftIO . putStrLn $ 
        "Ethereum transaction ID: " ++ ethTXID ++
        "\nFae transaction ID: " ++ show (getTXID txMessage)


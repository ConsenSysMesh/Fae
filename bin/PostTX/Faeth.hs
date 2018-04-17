module PostTX.Faeth where

import Blockchain.Fae.FrontEnd

import Common.Lens
import Common.ProtocolT

import Control.Monad.Trans

import PostTX.TXSpec

submitFaeth :: String -> Maybe Integer -> TXSpec -> IO ()
submitFaeth host faethEthValue TXSpec{specModules = LoadedModules{..}, ..} = do
  senderEthAccount <- readAccount "sender"
  EthAccount{address = faethEthAddress} <- readAccount "faeth"
  runProtocolT faethEthAddress $ do
    ethTXID <- handleAll err $ sendReceiveProtocolT 
      FaethTXData
      {
        faeTX = txMessage, 
        mainModule = snd mainModule, 
        ..
      }
    liftIO . putStrLn $ 
      "Ethereum transaction ID: " ++ ethTXID ++
      "\nFae transaction ID: " ++ show (getTXID txMessage)

  where err e = error $ "Ethereum client returned an error: " ++ show e

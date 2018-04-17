import Common.ProtocolT

import Control.Monad.Trans

import Data.Maybe

import PostTX.Args
import PostTX.Faeth
import PostTX.SpecParser
import PostTX.Submit
import PostTX.TXSpec
import PostTX.View

import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  userHome <- getHomeDirectory
  faeHome <- fromMaybe (userHome </> "fae") <$> lookupEnv "FAE_HOME"
  createDirectoryIfMissing True faeHome
  txDir <- getCurrentDirectory
  setCurrentDirectory faeHome

  args <- getArgs
  case parseArgs args of
    PostArgs{postArgFaeth = postArgFaeth@FaethArgs{..}, ..} -> do
      txData <- withCurrentDirectory txDir $ buildTXData postArgTXName
      txSpec <- txDataToSpec txData postArgFaeth
      if useFaeth 
      then submitFaeth postArgHost faethEthValue txSpec
      else submit postArgTXName postArgHost postArgFake postArgLazy txSpec
    ViewArgs{..} -> view viewArgTXID viewArgHost
    SenderArgs{..} -> runProtocolT nullAddress $ 
      case senderAddressM of
        Nothing -> do
          EthAccount{address} <- newAccount "sender" senderPassphrase
          liftIO $ putStrLn $ "New sender account: " ++ show address
        Just ethAddress -> do
          writeAccount "sender" 
            EthAccount
              {
                address = ethAddress,
                passphrase = senderPassphrase
              }
          liftIO $ putStrLn $ "Using sender account: " ++ show ethAddress


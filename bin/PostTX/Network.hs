module PostTX.Network where

import Blockchain.Fae.FrontEnd (collectTransaction, showTransaction, TXSummary)

import Common.JSON

import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe

import Network.HTTP.Client

requestURL :: String -> Request
requestURL host = fromMaybe (error $ "Bad host string: " ++ host) $ 
  parseRequest $ "http://" ++ host
    
-- | If JSON formatting is not enabled through postTX CLI flag
-- pretty print a summary of TX output.
sendReceive :: Bool -> Request -> IO ()
sendReceive jsonEnabled request = do
  manager <- newManager defaultManagerSettings
  response <- httpLbs request manager 
  let 
    txSummaryJSON = responseBody response
    txSummary = decodeJSON $ txSummaryJSON :: Maybe TXSummary
  if jsonEnabled
    then LC8.putStrLn txSummaryJSON
    else maybe (print txSummaryJSON) print txSummary

module PostTX.Network where

import Blockchain.Fae.FrontEnd (collectTransaction, showTXSummary, TXSummary)

import Common.JSON

import qualified Data.Aeson as A
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
    txSummary' = A.eitherDecode txSummaryJSON  :: Either String TXSummary
  if jsonEnabled
    then LC8.putStrLn txSummaryJSON
    else either (print . (++) "Failed To Parse TXSummary JSON: ") showTXSummary txSummary'

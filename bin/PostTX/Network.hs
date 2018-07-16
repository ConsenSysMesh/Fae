module PostTX.Network where

import Blockchain.Fae.FrontEnd (collectTransaction, TXSummary)

import Common.JSON

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe

import Network.HTTP.Client

import Text.PrettyPrint.HughesPJClass


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
    txSummaryE = A.eitherDecode txSummaryJSON  :: Either String TXSummary
  if jsonEnabled
    then LC8.putStrLn txSummaryJSON
    else putStrLn $ either ((++) "Failed To Parse TXSummary JSON: ") prettyShow txSummaryE

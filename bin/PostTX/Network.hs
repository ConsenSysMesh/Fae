module PostTX.Network where

import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe

import Network.HTTP.Client

requestURL :: String -> Request
requestURL host = fromMaybe (error $ "Bad host string: " ++ host) $ 
  parseRequest $ "http://" ++ host
    
sendReceive :: Request -> IO ()
sendReceive request = do
  manager <- newManager defaultManagerSettings
  response <- httpLbs request manager 
  LC8.putStrLn $ responseBody response

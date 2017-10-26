import Blockchain.Fae.Internal (public)

import qualified Data.ByteString as B
import qualified Data.Serialize as S

import System.Environment

main :: IO ()
main = do
  privKeyName : _ <- getArgs
  privKeyE <- S.decode <$> B.readFile privKeyName 
  let
    privKey =
      case privKeyE of
        Left e -> error $ 
          "Malformed private key: " ++ privKeyName ++ "\n" ++ show e
        Right x -> x
    pubKey =
      case public privKey of
        Nothing -> error $ "Invalid private key: " ++ privKeyName 
        Just x -> x
  print pubKey

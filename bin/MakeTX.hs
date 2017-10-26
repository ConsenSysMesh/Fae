import Blockchain.Fae.Internal 
  (TransactionID, ShortContractID(..), Signed(..), sign, digest, public)

import qualified Data.ByteString as B
import qualified Data.Serialize as S

import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  txModName : privKeyName : nonce : _ <- getArgs
  privKeyE <- S.decode <$> B.readFile privKeyName
  let 
    privKey =
      case privKeyE of
        Left e -> error $ 
          "Malformed private key: " ++ privKeyName ++ "\n" ++ show e
        Right x -> x
    txID = ShortContractID $ digest (public privKey, nonce)
    txIDName = "TX" ++ show txID
    txDir = "Blockchain" </> "Fae" </> "Transactions"
  txMod <- fmap (fixup txID) $ readFile $ txModName <.> "hs" 
  let signedTX = sign txMod privKey
  createDirectoryIfMissing True txDir
  writeFile (txDir </> txIDName <.> "hs") $ body signedTX
  B.writeFile (txDir </> txIDName <.> "signature") $ S.encode $ sig signedTX
  putStrLn $ show txID

fixup :: TransactionID -> String -> String
fixup txID txMod = (++ txMod) $
  "module Blockchain.Fae.Transactions.TX" ++ show txID ++ " where\n\n" ++
  "import Blockchain.Fae\n\n"

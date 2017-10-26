import Blockchain.Fae.Internal (Block(..), TX(..), Signed(..), unsign, digest) 

import Control.Monad

import qualified Data.ByteString as B
import qualified Data.Serialize as S

import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  txIDs <- getArgs
  let txDir = "Blockchain" </> "Fae" </> "Transactions"
  block <- fmap (Block []) $ forM txIDs $ \txIDS -> do
    let txName = "TX" ++ txIDS
    body <- readFile $ txDir </> txName <.> "hs"
    txSigE <- fmap S.decode $ B.readFile $ txDir </> txName <.> "signature" 
    let 
      sig = 
        case txSigE of
          Left e -> error $ 
            "Malformed transaction signature: " ++ txIDS ++ "\n" ++ show e
          Right x -> x
      pubKey = 
        case unsign Signed{..} of
          Nothing -> error $ "Incorrect transaction signature: " ++ txIDS
          Just pk -> pk
      txID = read txIDS
    return TX{..}
  let 
    blockDir = "Blockchain" </> "Fae" </> "Blocks"
    blockID = digest block
  createDirectoryIfMissing True blockDir
  B.writeFile (blockDir </> ("Block" ++ show blockID)) $ S.encode block
  print blockID


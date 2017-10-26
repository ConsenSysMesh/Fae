import Blockchain.Fae.Internal (runFaeInterpret, runBlock, showTransactions)

import Control.Monad.Trans

import Data.List

import qualified Data.ByteString as B
import qualified Data.Serialize as S

import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
  blockID : _ <- getArgs
  let blockDir = "Blockchain" </> "Fae" </> "Blocks"
  blockE <- fmap S.decode $ B.readFile $ blockDir </> ("Block" ++ blockID)
  let
    block =
      case blockE of
        Left e -> error $ "Malformed block: " ++ blockID ++ "\n" ++ show e
        Right x -> x
  txResults <- runFaeInterpret $ do
    runBlock block
    lift showTransactions
  putStrLn $ intercalate "\n\n" txResults


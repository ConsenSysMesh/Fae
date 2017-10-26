import Blockchain.Fae.Internal (unsafeNewPrivateKey)

import qualified Data.ByteString as B
import qualified Data.Serialize as S

import System.Environment

main :: IO ()
main = do
  privKeyName : _ <- getArgs
  B.writeFile privKeyName $ S.encode unsafeNewPrivateKey

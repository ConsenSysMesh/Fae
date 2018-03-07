import PostTX.Args
import PostTX.Submit
import PostTX.TXMessage

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let (txSpec, host, fake) = parseArgs args
  txData <- buildTXData txSpec
  submit txSpec host fake txData

